"a" | raco pkg install threading

Write-Output "Building Globals."
raco make src/globals.rkt

Write-Output "Building List library."
raco make src/collections/list.rkt

Write-Output "Building Array library."
raco make src/collections/array.rkt

Write-Output "Building Map library."
raco make src/collections/map.rkt

Write-Output "Building Seq library."
raco make src/collections/seq.rkt

Write-Output "Building IO Library."
raco make src/io/file.rkt
