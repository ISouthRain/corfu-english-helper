;;; corfu-english-helper.el --- English helper with corfu interface  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; English helper with corfu interface.
;;

;;; Require
(require 'cl-seq)
(require 'seq)
(require 'corfu)
(require 'corfu-english-helper-data)

;;; Code:

(defvar-local corfu-english-helper-active-p nil
  "The status of corfu-english-helper plugins. Default is disabled.")

(defvar-local corfu-english-helper-last-showing-candidate-max-index nil)
(defvar-local corfu-english-helper-last-input nil)
(defvar-local corfu-english-helper-translation-max-width nil)

;; 新增：用于备份开启前原本的 CAPF 列表和配置
(defvar-local corfu-english-helper--original-capf nil)
(defvar-local corfu-english-helper--corfu-auto nil)
(defvar-local corfu-english-helper--corfu-auto-prefix nil)

(defun corfu-english-helper-translation-max-width ()
  (let* ((showing-candidate-max-index (min (+ corfu-count corfu--scroll) corfu--total))
         (showing-candidate-min-index corfu--scroll))
    (if (and (equal corfu-english-helper-last-input corfu--input)
             (eql showing-candidate-max-index corfu-english-helper-last-showing-candidate-max-index))
        corfu-english-helper-translation-max-width
      (let* ((showing-candidates (seq-subseq corfu--candidates showing-candidate-min-index showing-candidate-max-index))
             ;; 优化 1: 使用 or 防护。合并后部分代码补全没有 :initials 属性，避免报错
             (widths (mapcar (lambda (c)
                               (string-width (or (get-text-property 0 :initials c) "")))
                             showing-candidates)))
        (setq corfu-english-helper-last-showing-candidate-max-index showing-candidate-max-index)
        (setq corfu-english-helper-last-input corfu--input)
        (setq corfu-english-helper-translation-max-width
              (if widths (apply #'max widths) 0))))))

(defun corfu-english-helper-annotation (candidate)
  (let* ((translation (or (get-text-property 0 :initials candidate) ""))
         (translation-width (string-width translation))
         (max-translation-width (or (corfu-english-helper-translation-max-width) 0))
         (blank-width (max 0 (- max-translation-width translation-width))))
    ;; 优化 2: 如果是普通代码补全（没有翻译），不显示多余空格
    (if (string-empty-p translation)
        ""
      (format "    %s%s" translation (make-string blank-width ?\s)))))

(defun corfu-english-helper-get-items (prefix)
  ;; 优化 4: 使用现代自带的 seq-filter 替代过时的 cl-remove-if-not
  (let* ((prefix-match-candidates
          (seq-filter
           (lambda (c) (string-prefix-p (downcase prefix) c))
           corfu-english-helper-completions)))
    (corfu-english-helper-convert-candidates prefix prefix-match-candidates)))

(defun corfu-english-helper-convert-candidates (input candidates)
  (cond ((corfu-english-helper-upcase-string-p input)
         (mapcar #'upcase candidates))
        ((corfu-english-helper-capitalize-string-p input)
         (mapcar #'capitalize candidates))
        (t candidates)))

(defun corfu-english-helper-upcase-string-p (str)
  (let ((case-fold-search nil))
    (and (> (length str) 1)
         (string-match-p "\\`[A-Z]*\\'" str))))

(defun corfu-english-helper-capitalize-string-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z][a-z]*\\'" str)))



(defun corfu-english-helper--make-combined-backend (main-backend)
  "合并 `main-backend' 和 English 补全, 并将 English 补全排在后面."
  (lambda ()
    (let ((main-res (funcall main-backend))
          (eng-res (corfu-english-helper-search)))
      (cond
       ;; 情况 A: 两个后端都有结果，进行分组排序融合
       ((and main-res eng-res)
        (let ((start (nth 0 main-res))
              (end (nth 1 main-res))
              (main-table (nth 2 main-res))
              (main-plist (nthcdr 3 main-res))
              (eng-table (nth 2 eng-res)))
          (list start end
                ;; 核心：实现自定义的补全表协议 (Completion Table Protocol)
                (lambda (str pred action)
                  (cond
                   ;; 1. 拦截元数据：要求外部 UI (如 Corfu) 保持我们提供的顺序，禁止重新打乱
                   ((eq action 'metadata)
                    '(metadata (category . mixed)
                               (display-sort-function . identity)
                               (cycle-sort-function . identity)))

                   ;; 2. 获取候选词并严格分组排序
                   ((eq action t)
                    (let* ((main-cands (all-completions str main-table pred))
                           (eng-cands (all-completions str eng-table pred))

                           ;; 提取原始代码补全自带的排序规则 (例如 LSP 自带的智能排序)
                           (main-meta (completion-metadata str main-table pred))
                           (main-sort-fn (completion-metadata-get main-meta 'display-sort-function))

                           ;; 降级排序方案：如果原生后端没有排序，使用 Corfu 的默认排序逻辑
                           (fallback-sort (if (bound-and-true-p corfu-sort-function)
                                              corfu-sort-function
                                            #'identity))
                           (final-main-sort (or main-sort-fn fallback-sort))

                           ;; 分别对两组候选词进行独立排序
                           (sorted-main (funcall final-main-sort main-cands))
                           (sorted-eng (funcall fallback-sort eng-cands)))

                      ;; 返回拼接结果：代码永远在前，英文永远在后
                      (append sorted-main sorted-eng)))

                   ;; 3. 处理 try-completion 动作 (当用户按下 TAB 尝试展开公共前缀时)
                   ((eq action nil)
                    (let ((m (try-completion str main-table pred))
                          (e (try-completion str eng-table pred)))
                      (if (and (stringp m) (stringp e)) m (or m e))))

                   ;; 4. 处理 test-completion 动作
                   ((eq action 'lambda)
                    (or (test-completion str main-table pred)
                        (test-completion str eng-table pred)))

                   (t nil)))

                ;; 融合属性：精确判断该显示代码注释还是中文翻译
                :annotation-function
                (lambda (cand)
                  ;; 如果 candidate 拥有 :initials 属性，说明它是英文词库里的单词
                  (if (get-text-property 0 :initials cand)
                      (corfu-english-helper-annotation cand)
                    ;; 否则，它是代码补全，安全地调用原生后端的 annotation-function
                    (let ((main-anno-fn (plist-get main-plist :annotation-function)))
                      (when main-anno-fn
                        (funcall main-anno-fn cand))))))))

       ;; 情况 B: 只有英文有结果
       (eng-res eng-res)

       ;; 情况 C: 只有原后端有结果
       (main-res main-res)

       ;; 情况 D: 都没有
       (t nil)))))


;;;###autoload
(defun corfu-english-helper-search (&optional interactive)
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'corfu-english-helper-search)))
        (completion-at-point))
    (let* ((bds (bounds-of-thing-at-point 'symbol))
           (start (car bds))
           (end (cdr bds)))
      ;; 优化 3: 确保 start 和 end 存在时才返回补全列表
      (when (and start end)
        (let ((prefix (buffer-substring-no-properties start end)))
          (list start end (corfu-english-helper-get-items prefix)
                :annotation-function #'corfu-english-helper-annotation))))))

;;;###autoload
(defun corfu-english-helper-toggle ()
  "切换: Corfu 仅有 English 单词补全."
  (interactive)
  (if corfu-english-helper-active-p
      (progn
        ;; Restore options.
        (setq-local corfu-auto corfu-english-helper--corfu-auto)
        (setq-local corfu-auto-prefix corfu-english-helper--corfu-auto-prefix)

        (setq-local completion-at-point-functions (remove 'corfu-english-helper-search completion-at-point-functions))
        (setq-local corfu-english-helper-active-p nil)
        (message "Corfu english helper has disable."))

    ;; Save options.
    (setq-local corfu-english-helper--corfu-auto corfu-auto)
    (setq-local corfu-english-helper--corfu-auto-prefix corfu-auto-prefix)

    ;; Turn on `corfu-auto' and adjust `corfu-auto-prefix' to 0.
    (setq-local corfu-auto t)
    (setq-local corfu-auto-prefix 0)

    ;; We need call `(setq-local corfu-auto t)' before corfu-mode turn on.
    (corfu-mode 1)

    (add-hook 'completion-at-point-functions #'corfu-english-helper-search nil t)
    (setq-local corfu-english-helper-active-p t)
    (message "Corfu english helper has enable.")))

;;;###autoload
(defun corfu-english-helper-toggle-capf ()
  "切换: Corfu 增添 English 单词补全."
  (interactive)
  (if corfu-english-helper-active-p
      (progn
        ;; 恢复原始状态
        (setq-local corfu-auto corfu-english-helper--corfu-auto)
        (when corfu-english-helper--original-capf
          (setq-local completion-at-point-functions corfu-english-helper--original-capf))
        (setq-local corfu-english-helper-active-p nil)
        (message "[corfu-english-helper] 已关闭."))

    ;; 备份当前配置
    (setq-local corfu-english-helper--corfu-auto corfu-auto)
    (setq-local corfu-english-helper--original-capf completion-at-point-functions)

    ;; 强制开启自动补全 (English Helper 核心需求)
    (setq-local corfu-auto t)
    (unless corfu-mode (corfu-mode 1))

    ;; --- 手动合并逻辑 ---
    (let ((current-main (car completion-at-point-functions)))
      (if (and current-main (not (eq current-main 'corfu-english-helper-search)))
          ;; 如果当前有主要补全源，创建一个复合源
          (setq-local completion-at-point-functions
                      (cons (corfu-english-helper--make-combined-backend current-main)
                            (cdr completion-at-point-functions)))
        ;; 如果当前没别的补全源，直接添加英文补全
        (add-hook 'completion-at-point-functions #'corfu-english-helper-search nil t)))

    (setq-local corfu-english-helper-active-p t)
    (message "[corfu-english-helper] 已开启.")))

(provide 'corfu-english-helper)

;;; corfu-english-helper.el ends here
