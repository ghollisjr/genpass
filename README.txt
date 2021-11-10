genpass is a simple SBCL script to generate passwords.

It depends on

* cl-getopt :https://www.github.com/ghollisjr/cl-getopt

and assumes a fixed location for the SBCL core file as written in the
hash-bang line at the top of the script as well as assuming that
.sbclrc has been setup for hash-bang scripts as follows:

.sbclrc:
----------------------------------------------------------------------
;;; If the first user-processable command-line argument is a filename,
;;;disable the debugger, load the file handling shebang-line and quit.
(let ((script (and (second *posix-argv*)
                   (probe-file (second *posix-argv*)))))
  (when script
    ;; Handle shebang-line
    (set-dispatch-macro-character #\# #\!
                                  (lambda (stream char arg)
                                    (declare (ignore char arg))
                                    (read-line stream)))
    ;; Disable debugger
    (setf *invoke-debugger-hook*
          (lambda (condition hook)
            (declare (ignore hook))
            ;; Uncomment to get backtraces on errors
            ;; (sb-debug:backtrace 20)
            (format *error-output* "Error: ~A~%" condition)
            (exit)))
    (load script)
    (exit)))
----------------------------------------------------------------------

See the example script included with cl-getopt to understand how to
build the SBCL core file, then place it somewhere you like and edit
genpass.lisp if necessary.

Run getopt.lisp with the "-h" option for usage.
