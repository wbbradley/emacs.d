(require 'flymake)


(defun flymake-mypy-init ()
  "Init mypy."
  (let* ((temp-file
          (flymake-init-create-temp-buffer-copy
           'flymake-create-temp-inplace))
         (local-file
          (file-relative-name temp-file
                              (file-name-directory
                               buffer-file-name))))
    (list "mypy" (list local-file "-s"))))


(when (load "flymake" t)
  (add-to-list
   'flymake-allowed-file-name-masks '("\\.py\\'" flymake-mypy-init)))
(provide 'flymake-mypy)
;;; flymake-mypy.el ends here
