;;; test.el --- test of git-messenger

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'git-messenger2)

(ert-deftest filter-pgp-signature ()
  "Strip PGP signature from in"
  (let* ((msg "hello
-----BEGIN PGP SIGNATURE-----
-----END PGP SIGNATURE-----
world")
         (filtered (git-messenger2--strip-pgp-signature msg)))
    (should (not (string-match-p "-----BEGIN PGP SIGNATURE-----" filtered)))
    (should (not (string-match-p "-----END PGP SIGNATURE-----" filtered)))))

;;; test.el ends here
