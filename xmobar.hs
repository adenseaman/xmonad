Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = False
       , persistent = False
       , hideOnStart = False
       , overrideRedirect = False
       , commands = [ Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%a %Y-%m-%d %H:%M:%S" "date" 10
                    , Run StdinReader
                    , Run Battery [  "-L" ,"40"
                                   , "-H" , "80"
                                   , "--high" , "#969896"
                                   , "--normal" , "#7aa6da"
                                   , "--low" , "#d54e53"
                                   , "-t" , "Bat: <left><acstatus>"
                                   , "--"
                                   , "-O", "<fc=#00ff00>+</fc>"
                                   , "-o", ""
                                   , "-i", ""
                                   ] 100
                    , Run Locks
                    , Run Com "/home/aden/bin/unread-counts.total.sh" ["/home/aden/.thunderbird/0nnsrq63.default"] "mail" 100
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ <fc=#ff0000>%locks%</fc> %mail% %cpu% | %memory% | %battery% | <fc=#ee9a00>%date%</fc>"
       }
