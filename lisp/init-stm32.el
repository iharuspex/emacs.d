;;; init-stm32.el
;;
;; Support for the STM32 MCU programming
;;
;; Author: iharuspex
;; Created: 16 Apr 2021
;; Based on stm32.el by Alexander Lutsai (https://github.com/SL-RU/stm32-emacs)
;;
;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

(defgroup stm32 nil                     
  "STM32 projects integration"
  :group 'development)

(defcustom stm32-st-util-command "st-util" 
  "The command to use to run st-util"
  :group 'stm32
  :type 'string)

(defcustom stm32-openocd-command "openocd"
  "The command to run openocd"
  :group 'stm32
  :type 'string)

(defcustom stm32-openocd-config-name "openocd.cfg"
  "OpenOCD requieres a .cfg file to file to properly function you need to
  provide the file, it must be called *stm32-openocd-config-name* and must be
  placed in the project's root directory"
  :group 'stm32
  :type 'string)

(defcustom stm32-cmd-make "make -j5"
  "Command to build project")

(defcustom stm32-cmd-cmake "cmake .."
  "Command to generate project in build directory"
  :group 'stm32
  :type 'string)

(defcustom stm32-gdb-start
  "arm-none-eabi-gdb -iex \"target extended-remote localhost:4242\" -i=mi "
  "Command to run gdb for gud"
  :group 'stm32
  :type 'string)

(defcustom stm32-build-compilation-database
  "compile_commands.json"
  "File in stm32-build-dir with compilation commands. You must to do
  set(CMAKE_EXPORT_COMPILE_COMMANDS 'ON')"
  :group 'stm32
  :type 'string)

(require 'cl-lib)
(require 'gdb-mi)
(require 'gud)
(require 'irony)

;(defun stm32-get-project-root-dir ()
;  "Return root path of current project"
  
