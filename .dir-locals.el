((nil . ((fill-column . 110)
         (eval . (progn
                   (use-package nl-scala-project :after scala-mode :load-path "~/.emacs.d/lisp")
                   (use-package bbweb-project :load-path "~/.emacs.d/lisp")
                   (setq default-directory (projectile-project-root)
                         dumb-jump-default-project (projectile-project-root)))))))
