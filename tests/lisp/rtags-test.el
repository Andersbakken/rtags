;; rtags-test.el --- Tests for rtags.el -*- lexical-binding: t ; -*-

;; Copyright (c) 2019 Christian Schwarzgruber

;; Author: Christian Schwarzgruber <c.schwarzgruber.cs@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:


;;; Code:

(require 'rtags)
(require 'ert)

(ert-deftest rtags-protocol-version-test ()
  "Test if the protocol version matches."
  (rtags-start-process-unless-running)
  (sit-for .5) ;; Uuh, that's nasty :)
  (rtags-call-rc "-w"))


;;; rtags-test.el ends here
