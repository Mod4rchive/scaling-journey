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
;dom: string X area de trabajo
;rec: historial
(define (build_history function_name_string workzone)
  (append (car workzone) (build_log function_name_string)))


;descripcion: Funcion que retorna un nuevo TDA con el historial modificado.
;dom: string X area de trabajo
;rec: area de trabajo
(define (set_history function_name workzone)
  (list (append (list (build_history function_name workzone)) (car workzone)) (cdr workzone)))


