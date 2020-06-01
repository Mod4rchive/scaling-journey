#lang racket
;zones = (history index localRepo remoteRepoDirectory)
;commit = (archivo1 ... archivoN)
;localRepo = (commit1 ... commitN)
;remoteRepo = ((branch1 commit1 ... commitN) (branchN commit 1 ... commit N))
;history = ((tiempoN funcionN) ... (tiempo1 funcion1))
;en orden cronologico


(define test1
  (list
   (list "HISTORY")
   (list "INDEX")
   (list "LOCAL REPO")
   (list (list "REMOTE REPO1")
         (list "REMOTE REPO2") ) ))

(define test2
  (list
   null
   null
   null
   (list (list "master"))))


(define (get_history workzone) (car workzone))

(define (get_index workzone) (cadr workzone))

(define (set_index new_index workzone)
  (list (get_history workzone) new_index (get_localRepo workzone) (get_remoteRepoDirectory workzone)))

(define (set_localRepo new_localRepo workzone)
  (list (get_history workzone) (get_index workzone) new_localRepo (get_remoteRepoDirectory workzone)))

(define (get_localRepo workzone) (caddr workzone))

(define (get_remoteRepoDirectory workzone) (cadddr workzone))

(define (get_currentRemoteRepo workzone) (car (cadddr workzone)))

(define (set_currentRemoteRepo new_currentRemoteRepo workzone)
  (list
   (get_history workzone)
   (get_index workzone)
   (get_localRepo workzone)
   (append (list new_currentRemoteRepo) (cdr (get_remoteRepoDirectory workzone)))))

(define (get_currentBranch workzone) (caar (cadddr workzone)))

;Constructor unidad  de historial (log).
;Recibe un string.
;Retorna un log (lista).
(define (build_log function_name_string)
  (list (number->string (current-seconds)) function_name_string))

;Actualiza el historial con un nuevo log.
;Recibe el area de trabajo y el log a añadir (listas).
;Retorna el historial actualizado (lista).
(define (build_history new_log workzone)
  (append (list (caar workzone)) (list new_log)))

;Modificador de historial.
;Recibe el nuevo historial y el area de trabajo (listas).
(define (set_history new_history workzone)
  (list (append (list new_history) (car workzone)) (cdr workzone)))

(define (set_remoteRepoDirectory new_remoteRepoDirectory workzone)
  (list (get_history workzone) (get_index workzone) (get_localRepo workzone) new_remoteRepoDirectory))

;Funcion git.
;Recibe un procedimiento y el area de trabajo (lista).
;Retorna la funcion del area de trabajo (lista).
(define git (lambda (function)
              (lambda (workzone)
                (function workzone))))

;Funcion envelope de add.
;Recibe una lista de archivos (lista de string) y el area de trabajo (lista).
;Retorna la funcion add-all del area de trabajo (lista). 
(define add (lambda (files)
              (lambda (workzone)
                (add-all files workzone))))

;Funcion que revisa la pertenencia de un elemento en una lista.
;Recibe el elemento a considerar y la lista a revisar.
;Retorna verdadero o falso dependiendo del caso.
(define (belongs? element list)
 (if [boolean? (member element list)]
     #f
     #t))

;Funcion add (recursion natural).
;Recide una lista de archivos (lista de strings) y el area de trabajo (lista).
;Retorna la funcion add del area de trabajo (lista).
(define (add-all files workzone)
  (if [null? files]
      workzone
      (if [belongs? (car files) (get_index workzone)]
          (add-all (cdr files) workzone)
          (add-all (cdr files)
                   (set_index (append (list (car files)) (get_index workzone)) workzone)))))

;Funcion commit.
;;Recibe un comentrario (string) y el area de trabajo (lista).
;Retorna la funcion commit del area de trabajo (lista).
(define (commit_envelope message workzone)
  (set_index null
             (set_localRepo
              (append (list (append (list message) (get_index workzone))) (get_localRepo workzone))
              workzone)))

;Funcion envelope de commit.
;Recibe un comentrario (string) y el area de trabajo (lista).
;Retorna la funcion commit del area de trabajo (lista).
(define commit (lambda (message)
                 (lambda (workzone)
                   (commit_envelope message workzone))))

;Funcion envelope de pull.
;Recibe  el area de trabajo (lista).
;Retorna la funcion pull del area de trabajo (lista).
(define (pull workzone)
  (set_localRepo
   (pull_envelope (cdr (get_currentRemoteRepo workzone)) (get_localRepo workzone) null) workzone))

;Funcion que ejerce la funcion pull por recursion de cola.
;Recibe  el repositorio remoto/local actual (lista) y una variable auxiliar (lista).
;Retorna la funcion pull del repositorio local (lista).
(define (pull_envelope remoteRepoCarry localRepoStatic ToBeAdded)
  (if [null? remoteRepoCarry]
      (append ToBeAdded localRepoStatic)
      (if [belongs? (car remoteRepoCarry) localRepoStatic]
          (pull_envelope (cdr remoteRepoCarry) localRepoStatic
                         ToBeAdded)
          (pull_envelope (cdr remoteRepoCarry) localRepoStatic
                         (append ToBeAdded (list (car remoteRepoCarry)))))))

;Funcion push.
;Recibe el area de trabajo (lista).
;Retorna la funcion push del area de trabajo (lista).
(define (push workzone)
  (set_currentRemoteRepo (append (get_currentRemoteRepo workzone) (get_localRepo workzone)) workzone))

;Transforma una lista de strings a un string separado por espacios para cada elemento.
;Recibe una lista.
;Retorna una representacion string de los elementos de la lista dada.
(define (list-->string list_)
    (if [null? list_]
        ""
        (apply string-append
               (map (lambda (list_string) (string-append list_string " ")) list_))))

;Transforma una representacion de repositorios remotos de lista a string.
;Recibe una lista y un string vacio.
;Retorna una representacion string de los elementos de la lista dada.
(define (remoteRepo->string remoteRepo stringCarry)
  (if [null? remoteRepo]
      stringCarry
      (remoteRepo->string (cdr remoteRepo) (string-append (list-->string (car remoteRepo))
                                                          stringCarry))))

;Funcion zonas->string.
;Recibe el area de trabajo (lista).
;Retorna una representacion string del area de trabajo con saltos de linea.
(define (zonas->string workzone)
  (apply string-append
         (list
          (list-->string (get_history workzone))
          "\n"
          (list-->string (get_index workzone))
          "\n"
          (list-->string (get_localRepo workzone))
          "\n"
          (remoteRepo->string (get_remoteRepoDirectory workzone) ""))))