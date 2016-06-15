(ns GraphNamedThings.test.entity_test
  (:require [GraphNamedThings.entity :refer :all]
            [clojure.test :refer :all]))

;Articles from http://archive.ics.uci.edu/ml/datasets/Reuters-21578+Text+Categorization+Collection


(def doc1a "The Tower Commission report, which
says President Reagan was ignorant about much of the Iran arms
deal, just about ends his prospects of regaining political
dominance in Washington, political analysts said.
    \"This is certification of incompetence,\" private political
analyst Stephen Hess told Reuters in commenting on the Tower
report made public today.
    \"It's as if he went before a professional licensing board
and was denied credentials.\"
    In one of the most direct criticisms, board chairman John
Tower, a longtime Reagan supporter and former Republican
senator from Texas, told a press conference, \"The president
clearly did not understand the nature of this operation.\"")

(def doc1b "The Tower Commission's scathing
comments on President Reagan's embattled chief of staff Donald
Regan could signal the death knell to his White House tenure,
but the impact of its strong criticism on two other top
officials was less clear.
    Regan has come in for tough criticism for his handling of
Reagan's worst political crisis since details of the covert
arms sales to Iran and diversion of profits to Nicaraguan
rebels first emerged last November.
    But criticism of the roles of Secretary of State George
Shultz and Defense Secretary Caspar Weinberger, who said they
opposed the Iran arms initiative yet failed to end it, had been
muted until the release of the Tower Commission report.")

(def doc2a "Valley Federal Savings and Loan
Association said it appointed Joseph Biafora to the post of
chairman and the company's president, Donald Headlund, was
named to the additional post of chief executive.
    The new appointments follow the death of former chairman
and chief executive Robert Gibson, the company said.
    It said Biafora had been vice chairman of the board.")



