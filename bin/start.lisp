(require :asdf)
(push #p"/home/dlowe/play/orca/" asdf:*central-registry*)
(require :swank)
(swank:create-server :port 40050 :style :spawn :dont-close t)
(require :orca)
(orca::orca-run "orca" "ita5prattle1.itasoftware.com"
                :port 6601
                :security :ssl)