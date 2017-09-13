(provide 'portacle-ag)
(ensure-installed 'ag)

;; Set the ag executable explicitly
(setq ag-executable (portacle-bin-path "ag"))
