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
 * Save status to data file.
 *
 * Keep the status so next time you bootup the service it wouldn't
 * forget what was the state last time we closed.
 */
function saveStatus() {
  let data = {
    pr_id: highestPRId,
    timestamp: new Date().toISOString().replace(/T/, ' ').replace(/\..+/, ''),
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
 * Load status from storage.
 *
 * Read the status data to revive our data to current service. The next
 * PR check will be the latest PR. Hence, we wouldn't make package review
 * twice.
 */
function loadStatus() {
  return new Promise((resolve, reject) => {
    fs.readFile(config.DATA_PATH, function (err, rw_data) {
      if (err)
        console.log('[ERROR] Error while loading status %s', err);
      else {
        console.log('[INFO] Reading status...');
        console.log('%s', rw_data);
        let data = JSON.parse(rw_data);
        highestPRId = data.pr_id;
      }
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
      return loadStatus();
    else
      return saveStatus();
  }).then(() => {  /* Register events. */
    return registerEvents();
  }).then(() => {
    console.log("\nDone loading :::");
  }).catch((err) => {
    console.log("\nInitializing fail :::", err);
  });
}
main();
