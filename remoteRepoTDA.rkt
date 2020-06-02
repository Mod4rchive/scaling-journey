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