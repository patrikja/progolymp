-- Uppgift 6 – Tankeläsning

testcases =
  [ (["- 1", "* 3", "+ 9", "/ 3", "+ 5", "- x"], "7")
  , (["- 1", "* 3", "+ 9", "/ 2", "+ 5", "- x"], "Nej")
  , (["+ 2", "- x", "* x", "/ 2", "+ 3", "- x"], "3")
  , (["+ 7", "* x", "* 0", "* x", "- 7"],        "-7")
  , (["* x"],                                    "Nej")
  , (["* 3", "/ 3", "- x", "+ 5"],               "5")
  , (["+ 1", "- x", "/ 2"],                      "Nej")
  ]

-- Just started
