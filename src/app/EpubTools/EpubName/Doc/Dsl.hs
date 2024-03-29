{-# LANGUAGE QuasiQuotes #-}

module EpubTools.EpubName.Doc.Dsl
   ( docs
   )
   where

import Text.Heredoc (here)


docs :: String
docs = [here|epubname rules documentation
----------------------------

Book naming is controlled by a set of rules loaded from a file 

At runtime, a rules file is searched for, first in the user's home directory and then in the installed files for epubname. The paths of these files specific to your system are listed in --help output.

The rules are checked, from first to last, for each file being renamed. The first rule that succeeds is used.


Rules have the following format:

   label
      authorMatch "..."
      subjectMatch "..."
      titlePat "..."
      name "..."

label is to identify what the rule is for to the user and also for verbose output (helpful for debugging rules)

The indented fields are rule commands and consist of:

   authorMatch - Optional. If present, it's a regexp pattern that must
      match an author in the book's metadata. If it doesn't match, this
      rule is rejected. This may be a partial match, "bar" will match if
      any author looks like "foo bar baz"

   subjectMatch - Optional. If present, it's a regexp pattern that must
      match a subject in the book's metadata. If it doesn't match, this
      rule is rejected. This may be a partial match, "bar" will match if
      any subject looks like "foo bar baz"

   titlePat - a regular expression for picking apart the title string in
      the book's metadata. The pieces in capture groups ( ) are exposed 
      as numbered matches in the name construction field. If this match
      fails, this rule is rejected.

   name - instructions for building up a new book filename, most often
      using the parts extracted in titlePat. More on this below


In the name command, there are several functions that can be used. These functions are delimited with parenthesis in the name command's expression.

   authors - Get formatted authors from the metadata. If there are any
      authors, a hyphen will be placed after them in the resulting string.

      syntax: (authors)

   INT - Retrieve the text of a specific numbered item from the title
      pattern match results

      syntax: (INT)

   monthNum INDEX - Construct a numeric month from a variety of English
      months strings. Including abbreviations and ranges

      syntax: (monthNum INT)

      examples:
         "January"           -> "01"
         "Feb"               -> "02"
         "Mar-Apr"           -> "03_04"
         "September/October" -> "09_10"

   pad WIDTH INDEX - Pad a numeric value from the pattern match results

      syntax: (pad INT INT)

      examples:
         2 "3"    -> "03"
         3 "10"   -> "010"

   publisher - Get the formatted publisher file-as string from the
      metadata

      syntax: (publisher)

   scrub INDEX - Clean up an item from the pattern match results. A
      large number of punctuation is removed and/or converted into
      other characters, and the string is CamelCased. In the end, only
      alphanumerics, - and _ are used.

      syntax: (scrub INT)

   wordNum WIDTH INDEX - Construct a numeric string from an English word
      for a number, with padding. Works on strings from "One" to "Twenty"

      syntax: (wordNum INT INT)

      example:
         2 "One"    -> "01"
         3 "Twenty" -> "020"

   year - Get the formatted publication year from the metadata

      syntax: (year)

   Anything else, not in parenthesis, is a string literal to be used at
   that position in the resulting string.


This probably seems confusing, some examples may help.

This one is a fictitious magazine with issue and date info in the title:

   original book metadata:
      title: Adventurer's Weekly Issue 53, May 2008

   epubname rule:
      magAdventureWeekly
         titlePat "(Adventurer's Weekly) Issue ([0-9]+), ([^ ]+) ([0-9]{4})$"
         name "(scrub 1)Magazine(pad 3 2)_(4)-(monthNum 3)"

   resulting title:
      "AdventurersWeeklyMagazine053_2008-05"


Here's one using some more functions:

   original book metadata:
      title: Thrilling Journeys Three
      creator
         role: aut
         text: The Thrilling Authors

   epubname rule:
      compThrillingJourneys
         author "Thrilling Authors"
         titlePat "(Thrilling Journeys) (.*)"
         name "(scrub 1)(wordNum 2 2)"

   resulting title:
      "ThrillingJourneys03"


And here's what happens when an ordinary (non-periodical) book is formatted:

   original book metadata:
      title: Springtime in the Village
      creator
         role: aut
         file-as: Dandridge, Suzanne
         text: Suzanne Dandridge
      date
         event: publication
         text: 2012-10-09

   epubname rule:
      ordinary_book
         titlePat "(.*)"
         name "(authors)(scrub 1)(year)"

   resulting title:
      "DandridgeSuzanne-SpringtimeInTheVillage_2012"


The last rule in the built-in rules, ordinary_book is very important, and looks like this:

   ordinary_book
      titlePat "(.*)"
      name "(authors)(scrub 1)(year)"

This rule will match any book and is used to format all simple author-title situations. This rules is responsible for the basic epubname behavior and should always be present and always be last.


Preceding a rule label with ! will "switch off" the entire rule. It's a simple way to comment out a single rule in its entirety. example:

   !compThrillingJourneys
      author "Thrilling Authors"
      titlePat "(Thrilling Journeys) (.*)"
      name "(scrub 1)(wordNum 2 2)"|]
