;; .lib files in this repo are Faust libraries (plain faust-mode: faustine's
;; check on save runs `faust FILE', which fails without a `process'); Faust
;; buffers load emacs/fau.el and turn on fau-mode
((nil
  . ((eval . (when (and buffer-file-name
                        (string-suffix-p ".lib" buffer-file-name)
                        (not (derived-mode-p 'faust-mode)))
               (faust-mode)))))
 (faust-mode
  . ((eval . (progn
               (load (expand-file-name "emacs/fau" (locate-dominating-file default-directory ".dir-locals.el")) nil t)
               (fau-mode 1))))))
