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
const data = require('./data');


// Use to store the newest PR Id. To check if we need to make any
// package review.
var new_pr_id = -1;


/**
 * Check if the command valid.
 * @param { string } cmd : Command to check installed.
 * @param { string } name : Name of the software to be prompt.
 */
function ensureCommand(cmd, name) {
  return new Promise((resolve, reject) => {
    exec(cmd, (error, stdout, stderr) => {
      if (error) {
        console.log('[ERROR] This program require %s to be installed %s', name, error.message);
        resolve();
      }
      if (stderr) {
        console.log('[ERROR] This program require %s to be installed %s', name, stderr);
        resolve();
      }
      console.log('[INFO] %s version checked.\n\n%s', name, stdout);
      resolve();
    });
  });
}

/**
 * Update the current status use to check if any packae needed to be check.
 */
function updateStatus() {
  return new Promise((resolve, reject) => {
    axios.get('https://api.github.com/repos/melpa/melpa/pulls')
      .then(response => {
        console.log(response.data);
      })
      .catch(error => {
        console.log(error);
      });
  });
}

/**
 * Main loop to check any PR need to be review.
 */
function doCheckPR() {
  return new Promise((resolve, reject) => {
    console.log("Start checking PR package :::");
    resolve();
  }).then(() => {  /* Update PR status. */
    return updateStatus();
  }).then(() => {
    console.log("\nDone loading :::");
  }).catch((err) => {
    console.log("\nFailed check PR while processing :::", err);
  });
}

/**
 * Register any events you want here.
 */
function registerEvents() {
  return new Promise((resolve, reject) => {
    console.log('[INFO] Register event action');
    {
      setInterval(doCheckPR, config.CHECK_TIME);
      console.log('  - [ ] Register check PR event');
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
  }).then(() => {  /* Ensure Git installed. */
    return ensureCommand('git --version', 'Git');
  }).then(() => {  /* Ensure Emacs installed. */
    return ensureCommand('emacs --version', 'Emacs');
  }).then(() => {  /* Prepare data file. */
    if (fs.existsSync(config.DATA_PATH))
      return data.loadStatus();
    else
      return data.saveStatus();
  }).then(() => {  /* Register events. */
    return registerEvents();
  }).then(() => {
    console.log("\nDone loading :::");
    if (config.DEBUG) doCheckPR();
  }).catch((err) => {
    console.log("\nInitializing fail :::", err);
  });
}
main();
