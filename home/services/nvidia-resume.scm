(define-module (home services nvidia-resume)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages wm)
  #:use-module (guix gexp)
  #:export (home-nvidia-resume-service-type))

(define nvidia-resume-script
  (program-file "nvidia-resume"
    #~(begin
        (use-modules (ice-9 popen)
                     (ice-9 rdelim))
        (let* ((hyprctl #$(file-append hyprland "/bin/hyprctl"))
               ;; Find the Hyprland instance signature from the runtime dir.
               (runtime-dir (or (getenv "XDG_RUNTIME_DIR")
                                (string-append "/run/user/"
                                               (number->string (getuid)))))
               (hypr-dir (string-append runtime-dir "/hypr"))
               (find-sig (lambda ()
                           (and (file-exists? hypr-dir)
                                (let* ((dir (opendir hypr-dir))
                                       (sig (let lp ()
                                              (let ((e (readdir dir)))
                                                (cond
                                                 ((eof-object? e) #f)
                                                 ((member e '("." "..")) (lp))
                                                 (else e))))))
                                  (closedir dir)
                                  sig))))
               ;; dbus-monitor prints one line per signal match; the body
               ;; contains "boolean true" (going to sleep) or "boolean false"
               ;; (waking up).
               (cmd (string-append
                     #$(file-append dbus "/bin/dbus-monitor")
                     " --system \"type='signal',"
                     "interface='org.freedesktop.login1.Manager',"
                     "member='PrepareForSleep'\""))
               (port (open-input-pipe cmd)))
          (let loop ()
            (let ((line (read-line port)))
              (unless (eof-object? line)
                (when (string-contains line "boolean false")
                  ;; System just resumed — cycle DPMS to wake NVIDIA outputs.
                  (let ((sig (find-sig)))
                    (when sig
                      (setenv "HYPRLAND_INSTANCE_SIGNATURE" sig)
                      (system* hyprctl "dispatch" "dpms" "off")
                      (sleep 2)
                      (system* hyprctl "dispatch" "dpms" "on"))))
                (loop))))))))

(define (home-nvidia-resume-shepherd-service _)
  (list
   (shepherd-service
    (provision '(nvidia-resume))
    (requirement '(wayland-compositor))
    (documentation
     "Listen for elogind resume signals and cycle DPMS to fix NVIDIA
black screen after suspend.")
    (start #~(lambda _
               ((make-forkexec-constructor
                 (list #$nvidia-resume-script)
                 #:environment-variables (environ)
                 #:log-file
                 (string-append (getenv "XDG_STATE_HOME")
                                "/log/nvidia-resume.log")))))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    (respawn-limit #~'(3 . 10))
    (respawn-delay 5))))

(define home-nvidia-resume-service-type
  (service-type
   (name 'home-nvidia-resume)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-nvidia-resume-shepherd-service)))
   (default-value #f)
   (description
    "Monitor elogind PrepareForSleep signals and cycle Hyprland DPMS
on resume to work around NVIDIA display blanking after suspend.")))
