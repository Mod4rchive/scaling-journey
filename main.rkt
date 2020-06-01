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

;crea la unidad de historial. recibe el nombre de la funcion como string. se arreglara ingreso de
;datos despues.
(define (build_log function_name_string)
  (list (number->string (current-seconds)) function_name_string))

;actualiza el historial con una funcion dada. recibe una lista de log y el historial a sumar y retorna
;el historial con el log añadido.
(define (build_history new_log workzone)
  (append (list (caar workzone)) (list new_log)))

;añade el historial actualizado al TDA zonas de la funcion realizada. recibe el historial actualizado
;(lista), el tda zonas y retorna el tda zonas con el historial actualizado.
(define (set_history new_history workzone)
  (list (append (list new_history) (car workzone)) (cdr workzone)))

(define (set_remoteRepoDirectory new_remoteRepoDirectory workzone)
  (list (get_history workzone) (get_index workzone) (get_localRepo workzone) new_remoteRepoDirectory))


(define git (lambda (function)
              (lambda (workzone)
                (function workzone))))

(define add (lambda (files)
              (lambda (workzone)
                (add-all files workzone))))

(define (belongs? element list)
 (if [boolean? (member element list)]
     #f
     #t))

(define (add-all files workzone)
  (if [null? files]
      workzone
      (if [belongs? (car files) (get_index workzone)]
          (add-all (cdr files) workzone)
          (add-all (cdr files)
                   (set_index (append (list (car files)) (get_index workzone)) workzone)))))

(define (commit_envelope message workzone)
  (set_index null
             (set_localRepo
              (append (list (append (list message) (get_index workzone))) (get_localRepo workzone))
              workzone)))

(define commit (lambda (message)
                 (lambda (workzone)
                   (commit_envelope message workzone))))

(define (pull workzone)
  (set_localRepo
   (pull_envelope (cdr (get_currentRemoteRepo workzone)) (get_localRepo workzone) null) workzone))

(define (pull_envelope remoteRepoCarry localRepoStatic ToBeAdded)
  (if [null? remoteRepoCarry]
      (append ToBeAdded localRepoStatic)
      (if [belongs? (car remoteRepoCarry) localRepoStatic]
          (pull_envelope (cdr remoteRepoCarry) localRepoStatic
                         ToBeAdded)
          (pull_envelope (cdr remoteRepoCarry) localRepoStatic
                         (append ToBeAdded (list (car remoteRepoCarry)))))))

(define (push workzone)
  (set_currentRemoteRepo (append (get_currentRemoteRepo workzone) (get_localRepo workzone)) workzone))

(define (list-->string list_)
    (if [null? list_]
        ""
        (apply string-append
               (map (lambda (list_string) (string-append list_string " ")) list_))))

(define (remoteRepo->string remoteRepo stringCarry)
  (if [null? remoteRepo]
      stringCarry
      (remoteRepo->string (cdr remoteRepo) (string-append (list-->string (car remoteRepo))
                                                          stringCarry))))

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