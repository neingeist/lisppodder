(require :asdf)
(asdf:operate 'asdf:load-op :cl-ppcre)

(defparameter *proxy* nil)
(defparameter *cclan-mirror* "foo") ;; FIXME

(defun test ()
  (download-to-file "http://www.bl0rg.net" "foobar.tmp"))

(defun test-enclosures ()
  (dolist (enclosure-url (parse-enclosures "http://blogs.bl0rg.net/saugnapf/index.xml"))
    (format t "~A ~&" enclosure-url)))

(defun test-download-enclosures ()
  (download-enclosures "http://blogs.bl0rg.net/saugnapf/index.xml"))

(defun download-enclosures (url)
  (dolist (enclosure-url (parse-enclosures url))
    (download-to-file enclosure-url (url-filename enclosure-url))))

(defun parse-enclosures (url)
  (let ((enclosure-urls))
    (dolist (enclosure-url (cl-ppcre:all-matches-as-strings "url=(\'|\").*?(\'|\")" (download-to-string url)))
      (setf enclosure-url (cl-ppcre:regex-replace "url=(\'|\")(.*?)(\'|\")" enclosure-url "\\2"))
      (push enclosure-url enclosure-urls))
    enclosure-urls))

(defun url-filename (url)
  (cl-ppcre:regex-replace ".*/(.*?)$" url "\\1"))

(defun download-to-string (url)
  (with-output-to-string (out-stream)
    (destructuring-bind (response headers stream)
      (block got
        (destructuring-bind (response headers stream) (url-connection url)
          (unless (member response '(301 302))
            (return-from got (list response headers stream)))
            (close stream)
            (setf url (cdr (assoc :location headers)))))
      (cond
        ((>= response 400) (format t "Got error ~A from ~A" response url))
        (t (copy-stream stream out-stream)))
      (close stream))))  
  
(defun download-to-file (url file-name)
  (destructuring-bind (response headers stream)
    (block got
      (destructuring-bind (response headers stream) (url-connection url)
        (unless (member response '(301 302))
          (return-from got (list response headers stream)))
          (close stream)
          (setf url (cdr (assoc :location headers)))))
    (cond
      ((>= response 400) (format t "Got error ~A from ~A" response url))
      (t (with-open-file (o file-name :direction :output :if-exists :supersede)
        (copy-stream stream o))))
     (close stream)))
  

;; the following is shamelessy stolen from asdf-install

(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
    (let* ((port-start (position #\: url :start 7))
           (host-end (min (or (position #\/ url :start 7) (length url))
                          (or port-start (length url)))))
      (subseq url 7 host-end)))

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
    (let ((port-start (position #\: url :start 7)))
      (if port-start (parse-integer url :start (1+ port-start) :junk-allowed t) 80)))

(defun make-stream-from-url (url)
  #+:sbcl
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                          :type :stream
                          :protocol :tcp)))
    (sb-bsd-sockets:socket-connect
     s (car (sb-bsd-sockets:host-ent-addresses
             (sb-bsd-sockets:get-host-by-name (url-host url))))
     (url-port url))
    (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :full))
  #+:cmu
  (sys:make-fd-stream (ext:connect-to-inet-socket (url-host url) (url-port url))
                      :input t :output t :buffering :full))


(defun url-connection (url)
  (let ((stream (make-stream-from-url (or *proxy* url)))
        (host (url-host url)))
    (format stream "GET ~A HTTP/1.0~C~CHost: ~A~C~CCookie: CCLAN-SITE=~A~C~C~C~C"
            url #\Return #\Linefeed
            host #\Return #\Linefeed
            *cclan-mirror* #\Return #\Linefeed #\Return #\Linefeed)
    (force-output stream)
    (flet (#-:digitool
           (read-header-line ()
             (read-line stream))
           #+:digitool
           (read-header-line (&aux (line (make-array 16
                                                     :element-type 'character
                                                     :adjustable t
                                                     :fill-pointer 0))
                                   (byte nil))
             (print (multiple-value-bind (reader arg)
                        (ccl::stream-reader stream)
                      (loop (setf byte (funcall reader arg))
                            (case byte
                              ((nil)
                                (return))
                              ((#.(char-code #\Return)
                                  #.(char-code #\Linefeed))
                                (case (setf byte (funcall reader arg))
                                  ((nil #.(char-code #\Return) #.(char-code #\Linefeed)))
                                  (t (ccl:stream-untyi stream byte)))
                                (return))
                              (t
                                (vector-push-extend (code-char byte) line))))
                      (when (or byte (plusp (length line)))
                        line)))))
      (list
       (let* ((l (read-header-line))
              (space (position #\Space l)))
         (parse-integer l :start (1+ space) :junk-allowed t))
       (loop for line = (read-header-line)
             until (or (null line)
                       (zerop (length line))
                       (eql (elt line 0) (code-char 13)))
             collect
             (let ((colon (position #\: line)))
               (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                     (string-trim (list #\Space (code-char 13))
                                  (subseq line (1+ colon))))))
       stream))))

(defvar *stream-buffer-size* 8192)

(defun copy-stream (from to)
  "Copy into TO from FROM until end of the input stream, in blocks of
*stream-buffer-size*.  The streams should have the same element type."
  (unless (subtypep (stream-element-type to) (stream-element-type from))
    (error "Incompatible streams ~A and ~A." from to))
  (let ((buf (make-array *stream-buffer-size*
       :element-type (stream-element-type from))))
    (loop
     (let ((pos (sys:read-n-bytes from buf 0 *stream-buffer-size* nil)))
       (when (zerop pos) (return))
       (write-sequence buf to :end pos)))))
