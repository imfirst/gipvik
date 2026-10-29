;;; GIP-VIK VRF segment prototype, version 0.1
;;; Experimental: generates plan geometry only. Not an approved engineering calculation.
;;; Units: millimetres. Tested as source text only; AutoCAD runtime testing is still required.

(vl-load-com)

(setq *gipvik:app* "GIPVIK")
(setq *gipvik:version* "0.1")
(setq *gipvik:vrf-layer* "GIPVIK_VRF")
(setq *gipvik:drain-layer* "GIPVIK_DRAIN")
(setq *gipvik:offset* 25.0)
(setq *gipvik:unit-w* 818.0)
(setq *gipvik:unit-h* 230.0)
(setq *gipvik:pripo-size* 75.0)
(setq *gipvik:text-h* 150.0)
(setq *gipvik:id* 0)

;;; Initial catalog: wall-mounted Standard GSJC4 family visible in the template.
;;; Model / cooling kW / heating kW / W / H / D / liquid mm / gas mm / drain ID mm
(setq *gipvik:lg-wall-catalog*
  '(("ARNU05GSJC4" 1.6 1.8 818 316 189 6.35 12.7 16)
    ("ARNU07GSJC4" 2.2 2.5 818 316 189 6.35 12.7 16)
    ("ARNU09GSJC4" 2.8 3.2 818 316 189 6.35 12.7 16)
    ("ARNU12GSJC4" 3.6 4.0 818 316 189 6.35 12.7 16)
    ("ARNU15GSJC4" 4.5 5.0 818 316 189 6.35 12.7 16)))

(defun gipvik:ustr (s / i out ch hx)
  ;; Decode \U+XXXX sequences into AutoLISP Unicode strings.
  (setq i 1 out "")
  (while (<= i (strlen s))
    (if (and (= (substr s i 1) "\\")
             (= (substr s (+ i 1) 2) "U+")
             (>= (strlen s) (+ i 6)))
      (progn
        (setq hx (substr s (+ i 3) 4))
        (setq out (strcat out (chr (gipvik:hex4 hx))))
        (setq i (+ i 7)))
      (progn
        (setq out (strcat out (substr s i 1)))
        (setq i (1+ i))))
  )
  out)

(defun gipvik:hex4 (s / i n c v)
  (setq i 1 n 0)
  (while (<= i 4)
    (setq c (strcase (substr s i 1)))
    (setq v (cond ((and (>= (ascii c) 48) (<= (ascii c) 57)) (- (ascii c) 48))
                  ((and (>= (ascii c) 65) (<= (ascii c) 70)) (+ 10 (- (ascii c) 65)))
                  (T 0)))
    (setq n (+ (* n 16) v) i (1+ i)))
  n)

(defun gipvik:2d (p) (list (car p) (cadr p)))
(defun gipvik:xy+ (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(defun gipvik:xy- (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun gipvik:xy* (a k) (list (* (car a) k) (* (cadr a) k)))
(defun gipvik:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun gipvik:len (v) (sqrt (gipvik:dot v v)))
(defun gipvik:unit (v / n)
  (setq n (gipvik:len v))
  (if (> n 1e-9) (gipvik:xy* v (/ 1.0 n)) '(0.0 0.0)))
(defun gipvik:normal (a b / v)
  (setq v (gipvik:unit (gipvik:xy- b a)))
  (list (- (cadr v)) (car v)))
(defun gipvik:mid (a b) (gipvik:xy* (gipvik:xy+ a b) 0.5))
(defun gipvik:dist (a b) (distance (gipvik:2d a) (gipvik:2d b)))

(defun gipvik:next-id (prefix)
  (setq *gipvik:id* (1+ *gipvik:id*))
  (strcat prefix "-" (itoa *gipvik:id*)))

(defun gipvik:ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 name)
            '(70 . 0)
            (cons 62 color)
            '(6 . "Continuous"))))
  name)

(defun gipvik:tag (en kind id)
  (if en
    (progn
      (if (not (tblsearch "APPID" *gipvik:app*))
        (regapp *gipvik:app*))
      (entmod
        (append (entget en)
          (list
            (list -3
              (list *gipvik:app*
                    (cons 1000 kind)
                    (cons 1000 id)))))
      )
      (entupd en))
  en)

(defun gipvik:poly (pts layer closed kind id / data p en)
  (if (and pts (> (length pts) 1))
    (progn
      (setq data
        (list '(0 . "LWPOLYLINE")
              '(100 . "AcDbEntity")
              (cons 8 layer)
              '(100 . "AcDbPolyline")
              (cons 90 (length pts))
              (cons 70 (if closed 1 0))))
      (foreach p pts
        (setq data (append data (list (list 10 (car p) (cadr p)))))
      )
      (setq en (entmakex data))
      (gipvik:tag en kind id)))
  en)

(defun gipvik:mtext (pt value layer id / style en)
  (setq style (if (tblsearch "STYLE" "ls_200") "ls_200" (getvar "TEXTSTYLE")))
  (setq en
    (entmakex
      (list '(0 . "MTEXT")
            '(100 . "AcDbEntity")
            (cons 8 layer)
            '(100 . "AcDbMText")
            (list 10 (car pt) (cadr pt) 0.0)
            (cons 40 *gipvik:text-h*)
            '(41 . 3200.0)
            '(71 . 1)
            '(72 . 1)
            (cons 1 value)
            (cons 7 style)
            '(210 0.0 0.0 1.0))))
  (gipvik:tag en "TEXT" id))

(defun gipvik:leader (anchor labelpt value layer id / end elbow)
  ;; Simple editable polyline + MText, intentionally independent from MLeader styles.
  (setq end (list (- (car labelpt) 120.0) (+ (cadr labelpt) 40.0)))
  (setq elbow (list (car end) (cadr anchor)))
  (gipvik:poly (list anchor elbow end) layer nil "LEADER" (strcat id "-L"))
  (gipvik:mtext labelpt value layer (strcat id "-T")))

(defun gipvik:pick-label (prompt)
  (gipvik:2d (getpoint (gipvik:ustr prompt))))

(defun gipvik:label-or-auto (chosen anchor dx dy idx / sx)
  (if chosen
    chosen
    (progn
      (setq sx (if (= (rem idx 2) 0) 1.0 -1.0))
      (list (+ (car anchor) (* dx sx))
            (+ (cadr anchor) dy (* 180.0 idx)))))

(defun gipvik:pick-points (prompt / p out)
  (setq out nil)
  (while (setq p (getpoint (gipvik:ustr prompt)))
    (setq out (append out (list (gipvik:2d p)))))
  out)

(defun gipvik:butlast (items / out)
  (setq out nil)
  (while (cadr items)
    (setq out (append out (list (car items))))
    (setq items (cdr items)))
  out)

(defun gipvik:clean-points (items / out p)
  (setq out nil)
  (foreach p items
    (if (or (null out) (> (gipvik:dist (car (last out)) p) 0.5))
      (setq out (append out (list p))))
  out)

(defun gipvik:orthogonal (a b)
  ;; Default path is an L: horizontal first, then vertical. Add manual waypoints when needed.
  (gipvik:clean-points
    (list a (list (car b) (cadr a)) b)))

(defun gipvik:project-segment (p a b / ab ap ll t)
  (setq ab (gipvik:xy- b a)
        ap (gipvik:xy- p a)
        ll (gipvik:dot ab ab))
  (if (< ll 1e-8)
    a
    (progn
      (setq t (/ (gipvik:dot ap ab) ll))
      (setq t (max 0.0 (min 1.0 t)))
      (gipvik:xy+ a (gipvik:xy* ab t)))))

(defun gipvik:project-path (p path / i a b q best bestd d)
  (setq i 0 best (car path) bestd 1e99)
  (while (< i (1- (length path)))
    (setq a (nth i path)
          b (nth (1+ i) path)
          q (gipvik:project-segment p a b)
          d (gipvik:dist p q))
    (if (< d bestd) (setq best q bestd d))
    (setq i (1+ i)))
  best)

(defun gipvik:offset-points (pts amount / out n i p n1 n2 av den fac)
  ;; Mitered offset polyline; the paired tube centre-lines remain 50 mm apart on straights.
  (setq out nil n (length pts) i 0)
  (while (< i n)
    (cond
      ((= i 0)
        (setq av (gipvik:normal (nth i pts) (nth (1+ i) pts))
              fac amount))
      ((= i (1- n))
        (setq av (gipvik:normal (nth (1- i) pts) (nth i pts))
              fac amount))
      (T
        (setq n1 (gipvik:normal (nth (1- i) pts) (nth i pts))
              n2 (gipvik:normal (nth i pts) (nth (1+ i) pts))
              av (gipvik:unit (gipvik:xy+ n1 n2))
              den (gipvik:dot av n2))
        (if (< (abs den) 0.25)
          (setq av n2 den 1.0))
        (setq fac (/ amount den)))
    (setq p (nth i pts))
    (setq out (append out
      (list (list (+ (car p) (* (car av) fac))
                  (+ (cadr p) (* (cadr av) fac)))))
    (setq i (1+ i)))
  out)

(defun gipvik:pipe-pair (pts layer id / p1 p2)
  (setq pts (gipvik:clean-points pts))
  (if (> (length pts) 1)
    (progn
      (setq p1 (gipvik:offset-points pts *gipvik:offset*)
            p2 (gipvik:offset-points pts (- *gipvik:offset*)))
      (gipvik:poly p1 layer nil "PIPE-A" (strcat id "-A"))
      (gipvik:poly p2 layer nil "PIPE-B" (strcat id "-B")))))

(defun gipvik:rotate-offset (center ang off / x y)
  (setq x (car off) y (cadr off))
  (gipvik:xy+ center
    (list (- (* x (cos ang)) (* y (sin ang)))
          (+ (* x (sin ang)) (* y (cos ang))))))

(defun gipvik:unit-rectangle (center ang / hw hh)
  (setq hw (/ *gipvik:unit-w* 2.0)
        hh (/ *gipvik:unit-h* 2.0))
  (list
    (gipvik:rotate-offset center ang (list (- hw) (- hh)))
    (gipvik:rotate-offset center ang (list hw (- hh)))
    (gipvik:rotate-offset center ang (list hw hh))
    (gipvik:rotate-offset center ang (list (- hw) hh))))

(defun gipvik:nearest-edge (center ang target / mids p best bestd d)
  ;; Selects the closest of four edge midpoints as a provisional connection point.
  (setq mids
    (list
      (gipvik:rotate-offset center ang (list (/ *gipvik:unit-w* 2.0) 0.0))
      (gipvik:rotate-offset center ang (list (- (/ *gipvik:unit-w* 2.0)) 0.0))
      (gipvik:rotate-offset center ang (list 0.0 (/ *gipvik:unit-h* 2.0)))
      (gipvik:rotate-offset center ang (list 0.0 (- (/ *gipvik:unit-h* 2.0))))))
  (setq best (car mids) bestd 1e99)
  (foreach p mids
    (setq d (gipvik:dist p target))
    (if (< d bestd) (setq best p bestd d)))
  best)

(defun gipvik:square (center size layer id / h)
  (setq h (/ size 2.0))
  (gipvik:poly
    (list (list (- (car center) h) (- (cadr center) h))
          (list (+ (car center) h) (- (cadr center) h))
          (list (+ (car center) h) (+ (cadr center) h))
          (list (- (car center) h) (+ (cadr center) h)))
    layer T "PRIPO" id))

(defun gipvik:catalog-row (model)
  (assoc model *gipvik:lg-wall-catalog*))

(defun gipvik:has-generated (/ ss)
  (if (tblsearch "APPID" *gipvik:app*)
    (setq ss (ssget "_X" (list (list -3 (list *gipvik:app*))))))
  (and ss (> (sslength ss) 0)))

(defun gipvik:clear-generated (/ ss i n)
  (setq n 0)
  (if (tblsearch "APPID" *gipvik:app*)
    (if (setq ss (ssget "_X" (list (list -3 (list *gipvik:app*)))))
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (if (entdel (ssname ss i)) (setq n (1+ n)))
          (setq i (1+ i)))))
  n)

(defun gipvik:store-unit
  (idx model center angle mainpath drainpath / row rctrl dctrl rguess dguess rjoin djoin rport dport rpath dpath lab-unit lab-pripo lab-r lab-d)
  (setq row (gipvik:catalog-row model))
  (setq rctrl
    (gipvik:pick-points
      (strcat
        (gipvik:ustr "\\U+0412\\U+0435\\U+0442\\U+0432\\U+044C LG: \\U+043A\\U+043B\\U+0438\\U+043A\\U+0430\\U+0439\\U+0442\\U+0435 \\U+043F\\U+043E\\U+0432\\U+043E\\U+0440\\U+043E\\U+0442\\U+044B; \\U+043F\\U+043E\\U+0441\\U+043B\\U+0435\\u0434\\u043D\\u044F\\u044F \\u0442\\u043E\\u0447\\u043A\\u0430 \\u2014 \\u043C\\u0435\\u0441\\u0442\\u043E \\u0440\\u0435\\u0444\\u043D\\u0435\\u0442\\u0430. Enter \\u2014 \\u0430\\u0432\\u0442\\u043E\\u043C\\u0430\\u0440\\u0448\\u0440\\u0443\\u0442: "))))
  (setq rguess (if rctrl (car (last rctrl)) center)
        rjoin (gipvik:project-path rguess mainpath)
        rport (gipvik:nearest-edge center angle rjoin))
  (setq rpath
    (if rctrl
      (gipvik:clean-points (append (list rport) (gipvik:butlast rctrl) (list rjoin)))
      (gipvik:orthogonal rport rjoin)))
  (setq dctrl
    (gipvik:pick-points
      (gipvik:ustr "\\U+0414\\U+0440\\U+0435\\U+043D\\U+0430\\U+0436: \\U+043F\\U+043E\\U+0432\\U+043E\\U+0440\\u043E\\u0442\\u044B \\u043A \\u043C\\u0430\\u0433\\u0438\\u0441\\u0442\\u0440\\u0430\\u043B\\u0438; \\u043F\\u043E\\u0441\\u043B\\u0435\\u0434\\u043D\\u044F\\u044F \\u0442\\u043E\\u0447\\u043A\\u0430 \\u2014 \\u043F\\u043E\\u0434\\u043A\\u043B\\u044E\\u0447\\u0435\\u043D\\u0438\\u0435. Enter \\u2014 \\u0430\\u0432\\u0442\\u043E\\u043C\\u0430\\u0440\\u0448\\u0440\\u0443\\u0442: ")))
  (setq dguess (if dctrl (car (last dctrl)) center)
        djoin (gipvik:project-path dguess drainpath)
        dport (gipvik:nearest-edge center angle djoin))
  (setq dpath
    (if dctrl
      (gipvik:clean-points (append (list dport) (gipvik:butlast dctrl) (list djoin)))
      (gipvik:orthogonal dport djoin)))
  (setq lab-unit
    (gipvik:pick-label
      "\\U+0412\\U+044B\\U+043D\\U+043E\\U+0441\\u043A\\u0430 \\u043C\\u0430\\u0440\\u043A\\u0438 \\u0431\\u043B\\u043E\\u043A\\u0430 (Enter = \\u0430\\u0432\\u0442\\u043E): "))
  (setq lab-pripo
    (gipvik:pick-label
      "\\U+0412\\U+044B\\U+043D\\u043E\\u0441\\u043A\\u0430 PRIPO (Enter = \\u0430\\u0432\\u0442\\u043E): "))
  (setq lab-r
    (gipvik:pick-label
      "\\U+0412\\U+044B\\u043D\\u043E\\u0441\\u043A\\u0430 \\u0434\\u0438\\u0430\\u043C\\u0435\\u0442\\u0440\\u043E\\u0432 \\u0444\\u0440\\u0435\\u043E\\u043D\\u043E\\u0432\\u043E\\u0439 \\u0432\\u0435\\u0442\\u0432\\u0438 (Enter = \\u0430\\u0432\\u0442\\u043E): "))
  (setq lab-d
    (gipvik:pick-label
      "\\U+0412\\u044B\\u043D\\u043E\\u0441\\u043A\\u0430 \\u0434\\u0440\\u0435\\u043D\\u0430\\u0436\\u0430 (Enter = \\u0430\\u0432\\u0442\\u043E): "))
  (list
    (cons "idx" idx)
    (cons "model" model)
    (cons "row" row)
    (cons "center" center)
    (cons "angle" angle)
    (cons "rjoin" rjoin)
    (cons "rport" rport)
    (cons "rpath" rpath)
    (cons "djoin" djoin)
    (cons "dport" dport)
    (cons "dpath" dpath)
    (cons "lab-unit" lab-unit)
    (cons "lab-pripo" lab-pripo)
    (cons "lab-r" lab-r)
    (cons "lab-d" lab-d)))

(defun gipvik:val (item key) (cdr (assoc key item)))

(defun gipvik:draw-unit (u liquid drain / idx model center angle row rport rjoin rpath dport djoin dpath pripo pripo-id mid-r mid-d lab)
  (setq idx (gipvik:val u "idx")
        model (gipvik:val u "model")
        center (gipvik:val u "center")
        angle (gipvik:val u "angle")
        row (gipvik:val u "row")
        rport (gipvik:val u "rport")
        rjoin (gipvik:val u "rjoin")
        rpath (gipvik:val u "rpath")
        dport (gipvik:val u "dport")
        djoin (gipvik:val u "djoin")
        dpath (gipvik:val u "dpath"))
  (gipvik:poly (gipvik:unit-rectangle center angle) *gipvik:vrf-layer* T "INDOOR-UNIT" (gipvik:next-id "UNIT"))
  (gipvik:pipe-pair rpath *gipvik:vrf-layer* (gipvik:next-id "REF-BRANCH"))
  (gipvik:poly dpath *gipvik:drain-layer* nil "DRAIN-BRANCH" (gipvik:next-id "DRAIN-BRANCH"))
  (setq pripo (gipvik:xy+ rport (gipvik:xy* (gipvik:xy- rjoin rport) 0.18)))
  (gipvik:square pripo *gipvik:pripo-size* *gipvik:vrf-layer* (gipvik:next-id "PRIPO"))
  (setq lab
    (gipvik:label-or-auto (gipvik:val u "lab-unit") center
      (if (= (rem idx 2) 0) -1900.0 1000.0) 600.0 idx))
  (gipvik:leader center lab
    (strcat (gipvik:ustr "\\U+0412\\U+043D\\U+0443\\U+0442\\U+0440\\U+0435\\U+043D\\U+043D\\U+0438\\U+0439 \\U+0431\\U+043B\\U+043E\\U+043A LG ") model)
    *gipvik:vrf-layer* (gipvik:next-id "UNIT-LABEL"))
  (setq lab
    (gipvik:label-or-auto (gipvik:val u "lab-pripo") pripo
      (if (= (rem idx 2) 0) 900.0 -2100.0) -500.0 (+ idx 1)))
  (gipvik:leader pripo lab
    (gipvik:ustr "\\U+041C\\U+043E\\U+0434\\U+0443\\U+043B\\u044C LG PRIPO")
    *gipvik:vrf-layer* (gipvik:next-id "PRIPO-LABEL"))
  (setq mid-r (gipvik:mid rport rjoin))
  (setq lab
    (gipvik:label-or-auto (gipvik:val u "lab-r") mid-r
      (if (= (rem idx 2) 0) 1100.0 -2300.0) 200.0 (+ idx 2)))
  (gipvik:leader mid-r lab
    (strcat "%%C6,35 / %%C12,7" "\\P" (gipvik:ustr "\\U+043F\\U+043E\\U+0434 \\U+043F\\U+043E\\U+0442\\U+043E\\U+043B\\U+043A\\U+043E\\U+043C"))
    *gipvik:vrf-layer* (gipvik:next-id "REF-LABEL"))
  (setq mid-d (gipvik:mid dport djoin))
  (setq lab
    (gipvik:label-or-auto (gipvik:val u "lab-d") mid-d
      (if (= (rem idx 2) 0) -1800.0 900.0) -450.0 (+ idx 3)))
  (gipvik:leader mid-d lab
    (strcat (gipvik:ustr "\\U+0414\\U+0440\\U+0435\\U+043D\\U+0430\\u0436") " %%C" drain)
    *gipvik:drain-layer* (gipvik:next-id "DRAIN-LABEL"))
  (prompt
    (strcat
      (gipvik:ustr "\\n\\U+0414\\U+043E\\U+0431\\U+0430\\u0432\\u043B\\u0435\\u043D LG ")
      model
      " | " (rtos (nth 1 row) 2 1) " kW / " (rtos (nth 2 row) 2 1) " kW"
      " | 818x316x189 mm | 6.35/12.7 mm"))
  (princ))

(defun c:GIPVRF (/ *error* olderr doc undo-started inlet main-extra mainpath outlet drain-extra drainpath
                     n i model-key model center angle unit units ans main-dia drain-dia row lab-in lab-dr
                     start-count)
  (setq olderr *error* undo-started nil units nil)
  (defun *error* (msg)
    (if (and undo-started doc)
      (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (setq undo-started nil *error* olderr)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (prompt (strcat "\nGIP-VIK prototype: " msg)))
    (princ))
  (prompt (strcat "\nGIP-VIK VRF segment / prototype " *gipvik:version*))
  (if (/= (getvar "INSUNITS") 4)
    (prompt
      (gipvik:ustr
        "\\n\\U+0412\\U+041D\\U+0418\\U+041C\\u0410\\u041D\\u0418\\u0415: INSUNITS \\u043D\\u0435 \\u0440\\u0430\\u0432\\u0435\\u043D 4 (\\u043C\\u0438\\u043B\\u043B\\u0438\\u043C\\u0435\\u0442\\u0440\\u044B). \\u041F\\u0440\\u043E\\u0432\\u0435\\u0440\\u044C\\u0442\\u0435 \\u0435\\u0434\\u0438\\u043D\\u0438\\u0446\\u044B \\u0447\\u0435\\u0440\\u0442\\u0435\\u0436\\u0430.")))
  (initget "D922 D952")
  (setq ans (getkword
    (gipvik:ustr
      "\\n\\U+0416\\u0438\\u0434\\u043A\\u043E\\u0441\\u0442\\u043D\\u0430\\u044F \\u0442\\u0440\\u0443\\u0431\\u0430 \\u043C\\u0430\\u0433\\u0438\\u0441\\u0442\\u0440\\u0430\\u043B\\u0438 [D922=9.22 / D952=9.52] <D922>: ")))
  (setq main-dia (if (= ans "D952") "9,52" "9,22"))
  (initget "D16 D20 D25")
  (setq ans (getkword
    (gipvik:ustr
      "\\n\\U+0415\\u0434\\u0438\\u043D\\u044B\\u0439 \\u0434\\u0438\\u0430\\u043C\\u0435\\u0442\\u0440 \\u0434\\u0440\\u0435\\u043D\\u0430\\u0436\\u0430 [D16/D20/D25] <D20>: ")))
  (setq drain-dia (cond ((= ans "D16") "16") ((= ans "D25") "25") (T "20")))
  (if (= main-dia "9,22")
    (prompt
      (gipvik:ustr
        "\\n\\U+041F\\u0440\\u0438\\u043D\\u044F\\u0442\\u043E 9,22 \\u043C\\u043C \\u043A\\u0430\\u043A \\u0432 \\u0448\\u0430\\u0431\\u043B\\u043E\\u043D\\u0435. \\u041F\\u0440\\u043E\\u0432\\u0435\\u0440\\u044C\\u0442\\u0435, \\u043D\\u0435 \\u0438\\u043C\\u0435\\u043B\\u043E\\u0441\\u044C \\u043B\\u0438 \\u0432 \\u0432\\u0438\\u0434\\u0443 9,52 \\u043C\\u043C (3/8 \\u0434\\u044E\\u0439\\u043C\\u0430).")))
  (setq inlet (getpoint
    (gipvik:ustr "\\n\\U+0423\\u043A\\u0430\\u0436\\u0438\\u0442\\u0435 \\u0442\\u043E\\u0447\\u043A\\u0443 \\u0432\\u0432\\u043E\\u0434\\u0430 VRF \\u0443 \\u0432\\u0445\\u043E\\u0434\\u043D\\u043E\\u0439 \\u0434\\u0432\\u0435\\u0440\\u0438: ")))
  (if inlet (setq inlet (gipvik:2d inlet)))
  (if inlet
    (progn
      (setq main-extra
        (gipvik:pick-points
          "\\n\\U+041C\\u0430\\u0433\\u0438\\u0441\\u0442\\u0440\\u0430\\u043B\\u044C: \\u0443\\u043A\\u0430\\u0436\\u0438\\u0442\\u0435 \\u043F\\u043E\\u0432\\u043E\\u0440\\u043E\\u0442\\u044B \\u0438 \\u043A\\u043E\\u043D\\u0435\\u0447\\u043D\\u044B\\u0439 \\u0440\\u0435\\u0444\\u043D\\u0435\\u0442; Enter \\u0437\\u0430\\u0432\\u0435\\u0440\\u0448\\u0438\\u0442: "))
      (setq mainpath (gipvik:clean-points (append (list inlet) main-extra))))
  )
  (if (and mainpath (> (length mainpath) 1))
    (progn
      (setq outlet (getpoint
        (gipvik:ustr "\\n\\U+0423\\u043A\\u0430\\u0436\\u0438\\u0442\\u0435 \\u0442\\u043E\\u0447\\u043A\\u0443 \\u0441\\u0431\\u0440\\u043E\\u0441\\u0430 \\u0434\\u0440\\u0435\\u043D\\u0430\\u0436\\u0430: ")))
      (if outlet (setq outlet (gipvik:2d outlet)))
      (if outlet
        (progn
          (setq drain-extra
            (gipvik:pick-points
              "\\n\\U+0414\\u0440\\u0435\\u043D\\u0430\\u0436\\u043D\\u0430\\u044F \\u043C\\u0430\\u0433\\u0438\\u0441\\u0442\\u0440\\u0430\\u043B\\u044C: \\u043A\\u043B\\u0438\\u043A\\u0430\\u0439\\u0442\\u0435 \\u0443\\u0437\\u043B\\u044B/\\u043F\\u043E\\u0432\\u043E\\u0440\\u043E\\u0442\\u044B; Enter \\u0437\\u0430\\u0432\\u0435\\u0440\\u0448\\u0438\\u0442: "))
          (setq drainpath (gipvik:clean-points (append (list outlet) drain-extra)))))))
  (if (and drainpath (> (length drainpath) 1))
    (progn
      (setq lab-in
        (gipvik:pick-label
          "\\U+0412\\u044B\\u043D\\u043E\\u0441\\u043A\\u0430 \\u043C\\u0430\\u0433\\u0438\\u0441\\u0442\\u0440\\u0430\\u043B\\u0438 \\u0445\\u043E\\u043B\\u043E\\u0434\\u0430 (Enter = \\u0430\\u0432\\u0442\\u043E): "))
      (setq lab-dr
        (gipvik:pick-label
          "\\U+0412\\u044B\\u043D\\u043E\\u0441\\u043A\\u0430 \\u0434\\u0440\\u0435\\u043D\\u0430\\u0436\\u043D\\u043E\\u0439 \\u043C\\u0430\\u0433\\u0438\\u0441\\u0442\\u0440\\u0430\\u043B\\u0438 (Enter = \\u0430\\u0432\\u0442\\u043E): "))
      (initget 7)
      (setq n (getint
        (gipvik:ustr "\\n\\U+0421\\u043A\\u043E\\u043B\\u044C\\u043A\\u043E LG VRF \\u0431\\u043B\\u043E\\u043A\\u043E\\u0432 \\u0434\\u043E\\u0431\\u0430\\u0432\\u0438\\u0442\\u044C (1-30): ")))
      (if (and n (<= n 30))
        (progn
          (setq i 1 units nil)
          (while (<= i n)
            (initget "ARNU05 ARNU07 ARNU09 ARNU12 ARNU15")
            (setq model-key
              (getkword
                (gipvik:ustr
                  (strcat "\\n\\U+041C\\u043E\\u0434\\u0435\\u043B\\u044C LG [ARNU05/ARNU07/ARNU09/ARNU12/ARNU15] <ARNU07> (\\U+0431\\u043B\\u043E\\u043A "
                          (itoa i) "): "))))
            (if (null model-key) (setq model-key "ARNU07"))
            (setq model (strcat model-key "GSJC4"))
            (setq center (getpoint
              (gipvik:ustr
                (strcat "\\n\\U+041A\\u043B\\u0438\\u043A\\u043D\\u0438\\u0442\\u0435 \\u0446\\u0435\\u043D\\u0442\\u0440 \\u0431\\u043B\\u043E\\u043A\\u0430 " model ": "))))
            (if center
              (progn
                (setq center (gipvik:2d center))
                (setq angle (getangle center
                  (gipvik:ustr "\\n\\U+0423\\u043A\\u0430\\u0436\\u0438\\u0442\\u0435 \\u043D\\u0430\\u043F\\u0440\\u0430\\u0432\\u043B\\u0435\\u043D\\u0438\\u0435 \\u0434\\u043B\\u0438\\u043D\\u043D\\u043E\\u0439 \\u0441\\u0442\\u043E\\u0440\\u043E\\u043D\\u044B \\u0431\\u043B\\u043E\\u043A\\u0430: ")))
                (if (null angle) (setq angle 0.0))
                (setq unit (gipvik:store-unit i model center angle mainpath drainpath))
                (setq units (append units (list unit)))
                (setq i (1+ i)))
              (setq i (+ n 1))))
          (if units
            (progn
              (if (gipvik:has-generated)
                (progn
                  (initget "Replace Cancel")
                  (setq ans (getkword
                    (gipvik:ustr
                      "\\n\\U+0412 \\u0447\\u0435\\u0440\\u0442\\u0435\\u0436\\u0435 \\u0435\\u0441\\u0442\\u044C \\u043E\\u0431\\u044A\\u0435\\u043A\\u0442\\u044B GIPVIK. \\u0417\\u0430\\u043C\\u0435\\u043D\\u0438\\u0442\\u044C \\u0438\\u0445? [Replace/Cancel] <Cancel>: "))))
                (if (/= ans "Replace") (setq units nil))
                (if (and units (= ans "Replace"))
                  (prompt
                    (strcat "\\nGIPVIK objects removed: "
                            (itoa (gipvik:clear-generated))))))
              (if units
                (progn
                  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
                  (vla-StartUndoMark doc)
                  (setq undo-started T)
                  (gipvik:ensure-layer *gipvik:vrf-layer* 5)
                  (gipvik:ensure-layer *gipvik:drain-layer* 7)
                  (regapp *gipvik:app*)
                  (gipvik:pipe-pair mainpath *gipvik:vrf-layer* (gipvik:next-id "REF-MAIN"))
                  (gipvik:poly drainpath *gipvik:drain-layer* nil "DRAIN-MAIN" (gipvik:next-id "DRAIN-MAIN"))
                  (gipvik:leader inlet
                    (gipvik:label-or-auto lab-in inlet 1300.0 400.0 0)
                    (strcat "%%C" main-dia " / %%C15,88" "\\P"
                      (gipvik:ustr "\\U+0432\\U+0432\\U+043E\\U+0434 \\U+043F\\U+043E\\U+0434 \\U+043F\\U+043E\\U+0442\\U+043E\\U+043B\\U+043A\\U+043E\\U+043C"))
                    *gipvik:vrf-layer* (gipvik:next-id "REF-INLET-LABEL"))
                  (gipvik:leader outlet
                    (gipvik:label-or-auto lab-dr outlet 1300.0 -450.0 1)
                    (strcat (gipvik:ustr "\\U+0414\\U+0440\\U+0435\\U+043D\\u0430\\u0436") " %%C" drain-dia)
                    *gipvik:drain-layer* (gipvik:next-id "DRAIN-OUTLET-LABEL"))
                  (setq i 0)
                  (foreach unit units
                    (gipvik:draw-unit unit main-dia drain-dia)
                    (setq i (1+ i)))
                  (vla-EndUndoMark doc)
                  (setq undo-started nil)
                  (prompt
                    (strcat
                      (gipvik:ustr "\\n\\U+0413\\U+0435\\U+043E\\u043C\\u0435\\u0442\\u0440\\u0438\\u044F \\u043F\\u043E\\u0441\\u0442\\u0440\\u043E\\u0435\\u043D\\u0430. \\u0411\\u043B\\u043E\\u043A\\u043E\\u0432: ")
                      (itoa (length units))
                      (gipvik:ustr ". \\U+0424\\u0440\\u0435\\u043E\\u043D\\u043E\\u0432\\u0430\\u044F \\u043C\\u0430\\u0433\\u0438\\u0441\\u0442\\u0440\\u0430\\u043B\\u044C: ")
                      main-dia "/15,88; "
                      (gipvik:ustr "\\U+0432\\U+0435\\U+0442\\U+0432\\U+0438: 6,35/12,7; \\U+0434\\U+0440\\U+0435\\U+043D\\u0430\\u0436 \\u0432\\u0435\\u0437\\u0434\\u0435 %%C")
                      drain-dia
                      (gipvik:ustr ". \\U+041E\\u0431\\u044A\\u0435\\u043A\\u0442\\u044B \\u043F\\u043E\\u043C\\u0435\\u0447\\u0435\\u043D\\u044B XData GIPVIK.")))
                  (prompt
                    (gipvik:ustr
                      "\\n\\U+0412\\u0430\\u0436\\u043D\\u043E: \\u044D\\u0442\\u043E \\u043F\\u0440\\u043E\\u0442\\u043E\\u0442\\u0438\\u043F \\u043F\\u043B\\u0430\\u043D\\u0430. \\u0410\\u043A\\u0441\\u043E\\u043D\\u043E\\u043C\\u0435\\u0442\\u0440\\u0438\\u044F, \\u0441\\u043F\\u0435\\u0446\\u0438\\u0444\\u0438\\u043A\\u0430\\u0446\\u0438\\u044F \\u0438 \\u043F\\u0440\\u043E\\u0432\\u0435\\u0440\\u043A\\u0438 \\u043E\\u0442\\u043C\\u0435\\u0442\\u043E\\u043A \\u0435\\u0449\\u0451 \\u043D\\u0435 \\u0440\\u0435\\u0430\\u043B\\u0438\\u0437\\u043E\\u0432\\u0430\\u043D\\u044B."))))
              (if (null units)
                (prompt
                  (gipvik:ustr
                    "\\n\\U+041F\\u043E\\u0441\\u0442\\u0440\\u043E\\u0435\\u043D\\u0438\\u0435 \\u043E\\u0442\\u043C\\u0435\\u043D\\u0435\\u043D\\u043E.")))))
      (prompt
        (gipvik:ustr
          "\\n\\U+041D\\u0435\\u043E\\u0431\\u0445\\u043E\\u0434\\u0438\\u043C\\u043E \\u0437\\u0430\\u0434\\u0430\\u0442\\u044C \\u043A\\u043E\\u043B\\u0438\\u0447\\u0435\\u0441\\u0442\\u0432\\u043E \\u0431\\u043B\\u043E\\u043A\\u043E\\u0432."))))
    (prompt
      (gipvik:ustr
        "\\n\\U+041C\\u0430\\u0433\\u0438\\u0441\\u0442\\u0440\\u0430\\u043B\\u044C \\u0434\\u043E\\u043B\\u0436\\u043D\\u0430 \\u0441\\u043E\\u0434\\u0435\\u0440\\u0436\\u0430\\u0442\\u044C \\u0445\\u043E\\u0442\\u044F \\u0434\\u0432\\u0435 \\u0442\\u043E\\u0447\\u043A\\u0438.")))
  (princ))

(defun c:GIPVRFCLEAR (/ n)
  (setq n (gipvik:clear-generated))
  (prompt
    (strcat
      (gipvik:ustr "\\n\\U+0423\\u0434\\u0430\\u043B\\u0435\\u043D\\u043E \\u043E\\u0431\\u044A\\u0435\\u043A\\u0442\\u043E\\u0432 GIPVIK: ")
      (itoa n)))
  (princ))

(prompt
  (strcat
    (gipvik:ustr "\\nGIP-VIK VRF prototype loaded. Commands: GIPVRF / GIPVRFCLEAR. ")
    "(v" *gipvik:version* ")"))
(princ)
