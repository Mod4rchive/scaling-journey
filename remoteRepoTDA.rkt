#lang racket
(provide (all-defined-out))

;Implementacion TDA repositorio remoto

;representacion
;(string X string ... string)
;(list branchName commit1 ... commitN) 

;SELECTOR
;descripcion: Funcion que retorna un repositorio remoto de nombre dado.
;dom: string X lista
;rec: repositorio remoto
(define (get_branch branchName remoteRepoDirectory)
  (if [null? remoteRepoDirectory]
      null
      (if [equal? (car remoteRepoDirectory) (branchName)]
          (car remoteRepoDirectory)
          (get_branch branchName (cdr remoteRepoDirectory)))))

;MODIFICADOR
;descripcion: Funcion que genera un nuevo repositorio remoto 
;  			  con los mismos elementos pero distinto nombre
;dom: repositorio remoto X string
;rec: lista
(define (set_branchName branchRepo newName)
  (append
   (list newName)
   (car (branchRepo))))

;OTRA FUNCION   
;descripcion: Funcion que genera una lista de strings separados por 
;			  espacios para cada elemento de una lista dada.
;dom: lista
;rec: string
(define (list--->string list_)
    (if [null? list_]
        ""
        (apply string-append
               (map (lambda (list_string) (string-append list_string " ")) list_))))


;descripcion: Funcion que genera una representacion de repositorios remotos de lista a string.
;dom: repositorio remoto X string
;rec: string
(define (remoteRepo->string remoteRepo stringCarry)
  (if [null? remoteRepo]
      stringCarry
      (remoteRepo->string (cdr remoteRepo) (string-append (list--->string (car remoteRepo))
                                                          stringCarry))))