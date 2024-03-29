{-# LANGUAGE QuasiQuotes #-}

module EpubTools.EpubName.Doc.Rules
   ( defaults
   )
   where

import Text.Heredoc (here)


defaults :: String
defaults = [here|magAeon
   titlePat "^A[eE]on ([^ ]+)$"
   name "AeonMagazine(wordNum 2 1)"

magApex
   titlePat "^(Apex)[^0-9]*([0-9]{1,3}).*"
   name "(scrub 1)Magazine(pad 3 2)"

magAnalog
   titlePat "^(A[^ ]*).*, ([^ ]+) ([0-9]{4})$"
   name "(scrub 1)SF(3)-(monthNum 2)"

magBcs
   titlePat "(Beneath Ceaseless.*) #([0-9]+).*"
   name "(scrub 1)_Issue(pad 3 2)"

magBlackStatic
   titlePat "^(Black Static Horror Magazine)[^0-9]*([0-9]+)$"
   name "(scrub 1)(pad 2 2)"

magChallengingDestiny
   titlePat "^(Challenging Destiny) #([0-9]+).*"
   name "(scrub 1)Magazine(pad 3 2)"

magClarkesworld
   titlePat "^(Clarkesworld)[^0-9]*([0-9]+)$"
   name "(1)(pad 3 2)"

magFantasyMag
   titlePat "^(Fantasy Magazine)[^0-9]+([0-9]+).*"
   name "(scrub 1)(pad 3 2)"

magEclipse_word
   titlePat "^(Eclipse) +([a-zA-Z]+)$"
   name "(1)(wordNum 2 2)"

magEclipse_num
   titlePat "^(Eclipse) +([0-9]+):.*"
   name "(1)(pad 2 2)"

magFsf
   authorMatch "Spilogale"
   titlePat ".* ([^ ]+) ([0-9]{4})$"
   name "FantasyScienceFiction(2)-(monthNum 1)"

magFutureOrbits
   titlePat "(Future Orbits) Issue ([0-9]+), ([^ ]+) ([0-9]{4})$"
   name "(scrub 1)Magazine(pad 2 2)_(4)-(monthNum 3)"

magGud
   authorMatch "GUD Magazine Authors"
   titlePat ".* Magazine Issue ([0-9]+) ::.*"
   name "GUDMagazine(pad 2 1)"

magInterzone
   titlePat "^(Interzone|INTERZONE)[^0-9]+([0-9]+).*"
   name "(scrub 1)SFF(pad 3 2)"

magLightspeed_date
   titlePat "(Lightspeed) Magazine, (.*) (.*)"
   name "(1)(3)-(monthNum 2)"

magLightspeed_issue
   titlePat "(Lightspeed).* Issue ([^ ]+)(.*)"
   name "(1)(pad 3 2)"

magLunaStationQuarterly
   titlePat "(Luna Station Quarterly) - Issue ([0-9]+)"
   name "(scrub 1)(pad 3 2)"

magNemesis
   titlePat "(Nemesis Mag)azine #([0-9]+).*"
   name "(scrub 1)(pad 3 2)"

magRageMachine
   titlePat "(Rage Machine.*)--([^ ]+) ([0-9]{4})$"
   name "(scrub 1)_(3)-(monthNum 2)"

magSomethingWicked
   titlePat "^(Something Wicked)[^0-9]*([0-9]+)"
   name "(scrub 1)(pad 3 2)"

magWeirdTales
   titlePat "^(Weird Tales)[^0-9]*([0-9]+)$"
   name "(scrub 1)(pad 3 2)"

magLocus
   titlePat "^(Locus), ([a-zA-Z]+) ([0-9]{4})$"
   name "(scrub 1)Magazine(3)-(monthNum 2)"

magGenericWithIssue
   titlePat "(.* Magazine):? Issue ([0-9]+).*"
   name "(scrub 1)(pad 2 2)"

magGenericSubjWithIssue
   subjectMatch "magazine"
   titlePat "(.*) Issue ([0-9]+).*"
   name "(scrub 1)Magazine(pad 2 2)"

magGenericVolNo3
   titlePat "(.*)Vol(ume)? ([0-9]{1,2}),? (No|Num)[^0-9]+([0-9]{3}).*"
   name "(scrub 1)Vol(pad 2 3)No(pad 3 5)"

magGenericVolNo2
   titlePat "(.*)Vol(ume)? ([0-9]{1,2}),? (No|Num)[^0-9]+([0-9]{1,2})[^0-9]?.*"
   name "(scrub 1)Vol(pad 2 3)No(pad 2 5)"

nonficWomensInstituteLibrary
   authorMatch "Woman's Institute"
   titlePat "(.*)"
   name "(scrub 1)"

anthology_date
   subjectMatch "anthology"
   titlePat "(.*) - ([^ ]+) ([0-9]{4})"
   name "(scrub 1)(3)-(monthNum 2)"

anthology
   subjectMatch "anthology"
   titlePat "(.*)"
   name "(scrub 1)(year)"

ordinary_book
   titlePat "(.*)"
   name "(authors)(scrub 1)(year)"|]
