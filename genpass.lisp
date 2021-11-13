#!/usr/bin/env -S sbcl --core ${HOME}/lib/sbcl-cores/cl-getopt.core --script
(import (list 'cl-getopt:getopt
              'cl-getopt:option-descriptions))

(defun codes->chars (codes)
  (map 'list #'code-char codes))

(defun range (start end)
  (loop
     for i from start to end
     collecting i))

(defun alphabet-lower-chars ()
  "Returns all lower-case alphabet characters"
  (codes->chars (range 97 122)))

(defun alphabet-upper-chars ()
  "Returns all upper-case alphabet characters"
  (codes->chars (range 65 90)))

(defun number-chars ()
  "Returns all numerical characters"
  (codes->chars (range 48 57)))

(defun space-chars ()
  "Returns all space characters"
  (list #\Space #\Tab))

(defun symbol-chars ()
  "Returns all symbol characters"
  (append (codes->chars (range 33 47))
          (codes->chars (range 58 64))
          (codes->chars (range 91 96))
          (codes->chars (range 123 126))))

(defun urandom-seed (&optional (nbits 32))
  (with-open-file (rand "/dev/urandom"
                        :direction :input
                        :element-type (list 'unsigned-byte nbits))
    (let* ((result (make-array 1 :element-type (list 'unsigned-byte nbits))))
      (read-sequence result rand)
      (elt result 0))))

(defparameter *state* nil)

(defun ensure-seed (&optional (nbits 32))
  (when (not *state*)
    (setf *state*
          (seed-random-state (urandom-seed nbits)))))

(defun rand (arg)
  (ensure-seed)
  (random arg *state*))

(defun genpass (length
                &key
                  (seed-nbits 64)
                  (lowercase-p t)
                  (uppercase-p t)
                  (number-p t)
                  (space-p t)
                  (symbol-p t)
                  (filter (constantly t))
                  (crack-cpu-speed 1d9))
  "genpass generates a random password for printable ASCII characters.
If lower-case-p or uppercase-p is NIL, no lower-case or upper-case
characters will be used, respectively.  Similarly for number-p.  If
space-p is NIL, then no whitespace characters will be used.  If
symbol-p is NIL, then no symbols will be used.

Values returned are the password string, size of charcter set,
password length, password entropy in bits based on the size of the
character set and password length, and the estimated amount of time in
years it would take to crack the password given the supplied
crack-cpu-speed which estimates the number of crack attempts per
second."
  (let* ((result (make-string length))
         (charset
          (remove-if-not
           filter
           (concatenate 'vector
                        (when uppercase-p
                          (alphabet-upper-chars))
                        (when lowercase-p
                          (alphabet-lower-chars))
                        (when number-p
                          (number-chars))
                        (when space-p
                          (space-chars))
                        (when symbol-p
                          (symbol-chars)))))
         (nchars (length charset))
         (entropy-bits (* length
                          (log nchars 2)))
         (crack-time (/ (expt 2d0 entropy-bits)
                        (* crack-cpu-speed 365.25d0 24.0d0 60.0d0 60.0d0))))
    (loop
       for i below length
       do (setf (elt result i)
                (aref charset
                      (rand nchars))))
    (values result
            nchars
            length
            entropy-bits
            crack-time)))

(defparameter *options*
  (list (list :short "h"
              :long "help"
              :description "show this help message"
              :argspec :none)
        (list :short "u"
              :long "uppercase"
              :description "disable uppercase characters"
              :argspec :none)
        (list :short "l"
              :long "lowercase"
              :description "disable lowercase characters"
              :argspec :none)
        (list :short "n"
              :long "numbers"
              :description "disable numerical characters"
              :argspec :none)
        (list :short "w"
              :long "whitespace"
              :description "disable whitespace"
              :argspec :none)
        (list :short "s"
              :long "symbols"
              :description "disable symbol characters"
              :argspec :none)
        (list :short "c"
              :long "cpu-speed"
              :description "CPU speed for entropy results"
              :argspec :required)
        (list :long "seed-nbits"
              :description "number of seed bits to read from /dev/urandom (default 64)"
              :argspec :required)))

(defun help ()
  (format t "Usage: genpass.lisp [options] <length>~%~%~a~%"
          (option-descriptions *options*
                               :column-width 17)))

(let* ((args sb-ext:*posix-argv*))
  (multiple-value-bind (options remaining)
      (getopt args *options*)
    (cond
      ((or (null (rest args))
           (null remaining))
       (help))
      ((gethash "h" options)
       (help))
      (t
       (let* ((length
               (read-from-string
                (first remaining))))
         (multiple-value-bind
               (password charsetsize length entropy cracktime)
             (apply #'genpass
                    length
                    ;; needs work
                    (append
                     (when (gethash "l" options)
                       (list :lowercase-p nil))
                     (when (gethash "u" options)
                       (list :uppercase-p nil))
                     (when (gethash "n" options)
                       (list :number-p nil))
                     (when (gethash "w" options)
                       (list :space-p nil))
                     (when (gethash "s" options)
                       (list :symbol-p nil))
                     ;; (filter (constantly t))
                     (when (gethash "c" options)
                       (let* ((speed
                               (read-from-string
                                (first (gethash "c" options))
                                nil nil)))
                         (list :crack-cpu-speed speed)))
                     (when (gethash "seed-nbits" options)
                       (let* ((nbits
                               (read-from-string
                                (first (gethash "seed-nbits" options))
                                nil nil)))
                         (list :seed-nbits nbits)))))
           (format t "~s~%~%Character set size: ~a~%Password length: ~a~%Entropy bits: ~a~%Crack time (years): ~a~%"
                   password charsetsize length entropy cracktime)))))))
