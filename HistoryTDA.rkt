#lang racket
(provide (all-defined-out))

;Implementacion TDA historial

;representacion
;(string X ... X string)
;(logN ... log1) 


;descripcion: Constructor unidad  de historial (log).
;dom: string
;rec: log
(define (build_log function_name_string)
  (list (number->string (current-seconds)) function_name_string))

;descripcion: Actualiza el historial con un nuevo log.
;dom: log X area de trabajo
;rec: historial
(define (build_history new_log workzone)
  (append (list (caar workzone)) (list new_log)))