(defun c:PipeFlow ( / *error* acadObj aDoc modelSpace linhas allowedTypes LINE_COLOR AREA_PROCURA_MAXIMA pipeLengthByLayer objectsAlreadyVerified
                       
                        ;;FUNCOES
                        detectaLinha chooseAColor get-rect-points
                       atualizarListaMaterial removeNotValidObjects
                       getCoords recursion showMaterial
                       )
  
  ;; FUNCOES
  (defun detectaLinha ( currentLine /  startPoint currentLayer)
    (setq startPoint (getCoords currentLine "START"))
    
    (setq vla_currentLine (vlax-ename->vla-object currentLine))
    (vla-put-color vla_currentLine LINE_COLOR)
    
    (setq objectsAlreadyVerified (append (list currentLine) objectsAlreadyVerified))
    
    (atualizarListaMaterial vla_currentLine)
    
    ;; Verificar a necessidade de usar o ZOOM, para não resultar em erros
    (removeNotValidObjects
      (ssdel currentLine 
            (ssget "_CP" 
                    (get-rect-points startPoint AREA_PROCURA_MAXIMA) 
                    allowedTypes
            )
      )
      startPoint
    )   
  )
  
  (defun chooseAColor ( / colour)
    (alert "Selecione uma cor para destacar as linhas.\nAtencao! Essa acao nao pode ser cancelada.")
    (setq colour nil)
    
    (while (null colour)
      (setq colour (cdr (car (acad_truecolordlg '(62 . 7) nil))))
    )
  )
  
  (defun get-rect-points (pt dist)
    (list
      (polar pt 2.3562 dist) ;; 135º
      (polar pt 0.785398 dist) ;; 45º
      (polar pt -0.785398 dist) ;; -45º
      (polar pt -2.3562 dist) ;; -135º
    )
  )
  
  (defun atualizarListaMaterial ( linhaAdicionar / currentLayer )
    (setq currentLayer (vla-get-layer vla_currentLine))
    
    (if (assoc currentLayer pipeLengthByLayer)
      ;; Se existir, atualize o valor.
      (setq pipeLengthByLayer
        ;; Atualiza o numero em textos iguais
        (subst
          ;; Valor novo
          (cons currentLayer (+ (cdr (assoc currentLayer pipeLengthByLayer)) (vla-get-length vla_currentLine)))
        
          ;; Valor Antigo
          (assoc currentLayer pipeLengthByLayer)

          ;; Lista        
          pipeLengthByLayer
        )
      )
    
      ;; Se não existir, adicionar no dicionario.
      (setq pipeLengthByLayer (append pipeLengthByLayer (list (cons currentLayer (vla-get-length vla_currentLine)))))
    )
  )

  (defun removeNotValidObjects (selectionList receivedStartPoint / currentObject currentStartPoint currentEndPoint i selectionList)
    (repeat (setq i (sslength selectionList))
      (setq currentObject (ssname selectionList (setq i (1- i))))  
      
      (setq currentStartPoint (getCoords currentObject "START"))
      (setq currentEndPoint (getCoords currentObject "END"))
      
      (cond
        ;; Verifica se essa linha já foi verifica anteriomente
        ((member currentObject objectsAlreadyVerified)
          (setq selectionList (ssdel currentObject selectionList))
        )
        
        ;; Verifica se o receivedStartPoint está mais perto do currentStartpoint do que a distancia de receivedStartPoint com o currentEndPoint
        ((< (distance receivedStartPoint currentStartPoint) (distance receivedStartPoint currentEndPoint))
          (setq selectionList (ssdel currentObject selectionList))
        )

        (t nil)
      )
    )
    
    selectionList
  )

  (defun getCoords (object point)
    (if (eq "START" point)
      (vlax-curve-getstartpoint (vlax-ename->vla-object object))
      (vlax-curve-getendpoint (vlax-ename->vla-object object))
    )
  )
  
  (defun recursion (listRec / i)
    (if (> (sslength listRec) 0)
      (repeat (setq i (sslength listRec))
        (recursion (detectaLinha (ssname listRec (setq i (1- i)))))
      )
    )
  )

  (defun showMaterial (pipeLengthByLayer / longestString i currentLength)
    (setq longestString 0)
    
    (repeat (setq i (length pipeLengthByLayer))
      (if (> (setq currentLength (strlen (car (nth (setq i (1- i)) pipeLengthByLayer)))) longestString)
        (setq longestString currentLength)
      )
    )
    
    (setq longestString (+ 20 longestString))
    
    (terpri)
    (repeat (fix (/ (- longestString 8) 2)) (princ "="))
    (princ " RESULT ")
    (repeat (fix (/ (- longestString 8) 2)) (princ "="))
    (prompt "LAYER")
    (repeat (- longestString 11) (princ "."))
    (princ "LENGTH")
    (terpri)
    
    (repeat longestString (princ "="))
    (terpri)
    
    (mapcar 
      (function
        (lambda (item)
          (prompt (car item))
          (repeat (- longestString (+ (strlen (car item)) (strlen (rtos (cdr item) 2 2)))) 
            (princ ".")
          )
          
          (princ (rtos (cdr item) 2 2))
        )
      )  
        pipeLengthByLayer
    )
    
    (terpri)
    (repeat longestString (princ "="))
    (terpri)
    
    (princ "Se o resultado nao sairam alinhados, isso significa que a fonte do seu console nao eh mono-espacada.\nPara uma melhor visualizacao. Por favor, considere a troca da fonte.")
    
    (princ)
  )

  (setq
    acadObj (vlax-get-acad-object)
    aDoc (vla-get-activedocument acadObj)
    modelSpace (vla-get-modelspace aDoc)
  )

  (vla-startundomark aDoc)
  
  (defun *error* (msg)
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
    
    (vla-endundomark aDoc)
  )
  
  (setq allowedTypes 
    '(
      (-4 . "<OR")  
      (0 . "LWPOLYLINE")
      (0 . "POLYLINE")
      (0 . "LINE")
      (-4 . "OR>")
    )
  )
  
  (setq pipeLengthByLayer (list))
  
  (setq objectsAlreadyVerified (list))
  
  (setq AREA_PROCURA_MAXIMA .2)
  (setq LINE_COLOR (chooseAColor))

  (setq linhas nil)

  (setq linhas 
         (while (null linhas)
          (setq linhas (ssget allowedTypes))
         )
  )
  
  (recursion linhas)
  
  (vla-endundomark aDoc)

  (terpri)
  (princ "Fim de execucao. Por favor, verifique os resultados. Podem existir casos onde: 
    1 - As Linhas foram desenhados na direcao errada.
    2 - A existencia de TEs (Linhas desenhadas no meio do tubo) [Numa proxima atualizacao, a LISP detecta-ra esse caso tambem]
Essas linhas nao sao consideradas nessa LISP. Verifique se algum desses foi o seu caso e faca os ajustes que julgar necessario.
  
Caso deseje que as cores retorne para suas cores originais. Pressione Ctrl + Z."
  )
  
  (showMaterial pipeLengthByLayer)

  (princ)
)

(alert "LISP Carregada com sucesso! Digite: \"PipeFlow\" para comecar.")