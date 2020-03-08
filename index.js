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

// Interval time to do PR review.
const CHECK_TIME = 1000;

// Record down the highest PR's ID use to check if there are any new PR.
var highestPRId = -1;


/**
 * Ensure Emacs is installed.
 */
function ensureEmacs() {
  return exec('emacs --version', (error, stdout, stderr) => {
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

/**
 * Update the current status use to check if any packae needed to be check.
 */
function updateStatus() {
  axios.get('https://api.github.com/repos/melpa/melpa/pulls')
    .then(response => {
      console.log(response.data);
    })
    .catch(error => {
      console.log(error);
    });
}

/**
 * Main loop to check any PR need to be review.
 */
function doCheckPR() {

}

/**
 * Program entry point.
 */
function main() {
  if (!ensureEmacs()) {
    console.log('[ERROR] This program require Emacs to be installed');
    return;
  }

  // Register events.
  setInterval(doCheckPR, CHECK_TIME);
}
main();
