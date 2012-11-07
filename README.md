# Synopsis

ghc-prof-mode is emacs major mode for viewing ghc profiling reports. This mode aims to make profiling more pleasant and less effort.

# Description

STUB

# Installing

Fetch package:
  cd ~/.emacs-libs/
  git clone https://github.com/fmap/ghc-prof-mode.git
Then add this to your .emacs file:
  (add-to-list 'load-path "~/.emacs-libs/ghc-prof-mode")
  (require 'ghc-prof-mode)
That's all.

# Key bindings 

Since the mode force report buffer to be readonly all keys in the mode are binded to one single button. So here is cheat sheet:

* 's' invokes ghc-prof-select-report. Sets currently focused report as current active report. 
* 'r' invokes ghc-prof-update-buffer. Update currently focused report from file.
* 'i' or '[' invokes ghc-prof-initiate-profiling. Start profiling with arguments from current report.
* 't' or ']' invokes ghc-prof-terminate-profiling. Terminate currently running profiling.
* 'h' invokes ghc-prof-highlight. Turn on indicators. 
* 'c' invokes ghc-prof-clear. Turn off indicators.
* 'g' or 'ret'  invokes ghc-prof-goto-function. Focus buffer to module and focus cursor in that module to function.
