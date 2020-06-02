#lang racket
(provide (all-defined-out))

;Implementacion TDA workzone (principal)

;representacion
;(lista X lista X lista X lista X lista)
;(list history workspace index localRepo remoteRepoDirectory) 

;SELECTORES
;descripcion: Funcion que retorna el historial de una zona de trabajo.
;dom: area de trabajo
;rec: lista
(define (get_history workzone) (car workzone))

;SELECTORES
;descripcion: Funcion que retorna el workspace de una zona de trabajo.
;dom: area de trabajo
;rec: lista
(define (get_workspace workzone) (cadr workzone))

;descripcion: Funcion que retorna el indice de una zona de trabajo.
;dom: area de trabajo
;rec: lista
(define (get_index workzone) (caddr workzone))

;descripcion: Funcion que retorna el repositorio local de una zona de trabajo.
;dom: area de trabajo
;rec: lista
(define (get_localRepo workzone) (caddr (cdr workzone)))

;descripcion: Funcion que retorna la lista de repositorios remotos de una zona de trabajo.
;dom: area de trabajo
;rec: lista de repositorios remotos
(define (get_remoteRepoDirectory workzone) (cadddr (cdr workzone)))

;descripcion: Funcion que retorna el repositorio remoto actual de una zona de trabajo.
;dom: area de trabajo
;rec: lista
(define (get_currentRemoteRepo workzone) (car (cadddr (cdr workzone))))

;descripcion: Funcion que retorna la rama actual de los commits de una zona de trabajo.
;dom: area de trabajo
;rec: string
(define (get_currentBranch workzone) (caar (cadddr (cdr workzone))))


;MODIFICADORES  
;descripcion:  Funcion que retorna un nuevo TDA con el indice modificado.
;dom: lista X area de trabajo
;rec: area de trabajo
(define (set_index new_index workzone)
  (list (get_history workzone)
        (get_workspace workzone)
        new_index
        (get_localRepo workzone)
        (get_remoteRepoDirectory workzone)))

;descripcion:  Funcion que retorna un nuevo TDA con el workspace modificado.
;dom: lista X area de trabajo
;rec: area de trabajo
(define (set_workspace new_workspace workzone)
  (list (get_history workzone)
        new_workspace
        (get_index workzone)
        (get_localRepo workzone)
        (get_remoteRepoDirectory workzone)))

;descripcion:  Funcion que retorna un nuevo TDA con la rama del repositorio remoto modificada.
;dom: lista X area de trabajo
;rec: area de trabajo
(define (set_currentRemoteRepo new_currentRemoteRepo workzone)
  (list
   (get_history workzone)
   (get_workspace workzone)
   (get_index workzone)
   (get_localRepo workzone)
   (append (list new_currentRemoteRepo) (cdr (get_remoteRepoDirectory workzone)))))
   
;descripcion:  Funcion que retorna un nuevo TDA con el repositorio local modificado.
;dom: lista X area de trabajo
;rec: area de trabajo
(define (set_localRepo new_localRepo workzone)
  (list (get_history workzone)
        (get_workspace workzone)
        (get_index workzone)
        new_localRepo
        (get_remoteRepoDirectory workzone)))

;descripcion:  Funcion que retorna un nuevo TDA con la lista de repositorios remotos modificada.
;dom: lista X area de trabajo
;rec: area de trabajo
(define (set_remoteRepoDirectory new_remoteRepoDirectory workzone)
  (list (get_history workzone)
        (get_workspace workzone)
        (get_index workzone)
        (get_localRepo workzone)
        new_remoteRepoDirectory))

;OTRAS FUNCIONES
;descripcion: Funcion que genera una lista de strings separados por 
;			  espacios para cada elemento de una lista dada.
;dom: lista
;rec: string
(define (list-->string list_)
    (if [null? list_]
        ""
        (apply string-append
               (map (lambda (list_string) (string-append list_string "\n")) list_))))

;descripcion: Funcion que genera una lista de strings separados por 
;			  espacios para cada elemento de una lista TDA repositorio remoto dada.
;dom: lista X string
;rec: string
;recursion: de cola
(define (remoteRepo->string list_ stringCarry)
    (if [null? list_]
        (string-append stringCarry "\n\n")
        (remoteRepo->string
         (cdr list_)
         (string-append stringCarry "\n" (list-->string (car list_))))
        ))

;descripcion: Funcion que revisa la pertenencia de un elemento en una lista.
;dom: cualquier elemento X lista
;recorrido: booleano
(define (belongs? element list)
 (if [boolean? (member element list)]
     #f
     #t))