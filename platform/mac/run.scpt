global pid

on forward(args)
  set cmd to quoted form of POSIX path of (path to current application)
  set cmd to cmd & "/Contents/Camp"
  repeat with arg in args
    set cmd to cmd & space & (quoted form of POSIX path of arg)
  end repeat
  set pid2 to do shell script cmd & " >/dev/null 2>&1 & echo $!"
  try
    pid
  on error
    set pid to pid2
  end try
end forward

on run
  forward({})
end run

on open args
  forward(args)
end open

on reopen args
  forward(args)
end open

on idle
  if pid is not "" then
    try
      do shell script "kill -0 " & pid
    on error
      set pid to ""
      quit
    end try
  end if
  return 1
end idle

on quit
  if pid is not "" then
    try
      do shell script "kill -15 " & pid
    on error
      continue quit
    end try
  end if
  continue quit
end quit
