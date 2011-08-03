(require :asdf)
(require :swank)
(swank:create-server :port 40050 :style :spawn :dont-close t)
(require :orca)
(orca::orca-run "orca" "ita2prattle1.itasoftware.com"
                :port 6601
                :security :ssl)
