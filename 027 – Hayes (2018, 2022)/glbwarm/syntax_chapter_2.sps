compute u=rv.uniform(0, 1).
sort cases by u.
temporary.
select if ($casenum < 51).
regression/dep = govact/method = enter negemot posemot ideology sex age.

regression/statistics defaults change/dep = govact/method = enter ideology sex age/method = enter negemot posemot.


compute d1 = (partyid = 1).
compute d2 = (partyid = 3).
regression/dep = govact/method = enter d1 d2.

