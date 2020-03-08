/**
 * $File: main.js $
 * $Date: 2020-03-07 23:55:25 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

const fs = require('fs');
const axios = require('axios');
const { exec } = require("child_process");

const config = require('./config');

// Record down the highest PR's ID use to check if there are any new PR.
var highestPRId = -1;


/**
 * Ensure Emacs is installed.
 */
function ensureEmacs() {
  return new Promise((resolve, reject) => {
    exec('emacs --version', (error, stdout, stderr) => {
      if (error) {
        console.log('[ERROR] This program require Emacs to be installed %s', error.message);
        resolve();
      }
      if (stderr) {
        console.log('[ERROR] This program require Emacs to be installed %s', stderr);
        resolve();
      }
      console.log('[INFO] Emacs version checked.\n\n%s', stdout);
      resolve();
    });
  });
}

/**
 * Save status to data file.
 */
function saveStatus() {
  let data = {
    pr_id: highestPRId,
    timestamp: new Date().toISOString(),
  };
  return new Promise((resolve, reject) => {
    fs.writeFile(config.DATA_PATH, JSON.stringify(data), function (err) {
      if (err)
        console.log('[ERROR] Error while saving data %s', err);
      else
        console.log("[INFO] Saved status file once => '%s'", config.DATA_PATH);;
      resolve();
    });
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
 * Register any events you want here.
 */
function registerEvents() {
  return new Promise((resolve, reject) => {
    console.log('[INFO] Register event action');
    {
      setInterval(doCheckPR, config.CHECK_TIME);
      console.log('  - Register check PR event');
    }
    resolve();
  });
}

/**
 * Program entry point.
 */
function main() {
  return new Promise((resolve, reject) => {
    console.log("Start initializing ::: (%s)", config.APP_NAME);
    resolve();
  }).then(() => {  /* Ensure Emacs installed. */
    return ensureEmacs();
  }).then(() => {  /* Prepare data file. */
    return saveStatus();
  }).then(() => {  /* Register events. */
    return registerEvents();
  }).then(() => {
    console.log("Done loading :::");
  }).catch((err) => {
    console.log("Initializing fail :::", err);
  });
}
main();
