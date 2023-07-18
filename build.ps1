"a" | raco pkg install threading

Write-Output "Building List library."
raco make collections/list.rkt

Write-Output "Building Array library."
raco make collections/array.rkt

Write-Output "Building Map library."
raco make collections/map.rkt

Write-Output "Building Seq library."
