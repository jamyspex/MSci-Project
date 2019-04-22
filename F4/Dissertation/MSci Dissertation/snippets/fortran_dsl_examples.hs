-- Generate a shift loop for use in smart cache
-- If loopVarName = "i" and sizeVarName = 
-- "bufferSize" the following code is generated:
--
-- !$PRAGMA unroll
-- do i = 1, bufferSize - 1, 1
--    ! loopBody code
-- end do
[ pragma "unroll"
, for 
    loopVarName 
    1 
    ((var sizeVarName) `minus` (con 1)) 
    loopBody
]

-- Generate a smart cache buffer shift line
-- If arrName = "eta" and loopVarName = "i"
-- the follow Fortran is generated:
--
-- eta_buffer[i] = eta_buffer[i+1]
(arrayVar 
  (arrName ++ "_buffer") 
  [var loopVarName])
`assign`
(arrayVar 
  (arrName ++ "_buffer") 
  [(var loopVarName) `plus` (con 1)])


