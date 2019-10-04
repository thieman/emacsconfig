(setq ag-highlight-search 't)
(setq ag-reuse-buffers 't)
(setq ag-reuse-window 't)
(setq compilation-scroll-output 'first-error)
(add-hook 'ag-search-finished-hook
          (lambda ()
            (pop-to-buffer next-error-last-buffer)))
