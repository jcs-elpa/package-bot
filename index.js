/**
 * $File: index.js $
 * $Date: 2020-03-07 23:55:25 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

const axios = require('axios');
const { exec } = require("child_process");

/**
 * Ensure Emacs is installed.
 */
function ensureEmacs() {
  exec('emacs --version', (error, stdout, stderr) => {
    if (error) {
      console.log(`error: ${error.message}`);
      return;
    }
    if (stderr) {
      console.log(`stderr: %{stderr}`);
      return;
    }
    console.log(`stdout: ${stdout}`);
  });
}

axios.get('https://api.github.com/repos/melpa/melpa/pulls')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.log(error);
  });
