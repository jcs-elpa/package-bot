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

const config = require('./config');
const data = require('./data');
const util = require('./util');


// Use to store the newest PR Id. To check if we need to make any
// package review.
var latest_pr_id = -1;


/**
 * Update the current status use to check if any packae needed to be check.
 */
function updateStatus() {
  return new Promise((resolve, reject) => {
    axios.get('https://api.github.com/repos/melpa/melpa/pulls')
      .then(response => {
        latest_pr_id = response.data[0].number;
        resolve();
      })
      .catch(error => {
        console.log('[ERROR] Error while updating PR status: %s', error);
        resolve();
      });
  });
}

/**
 * * Parse MELPA template.
 *
 * ```md
 * ### Brief summary of what the package does
 * , and more...
 * ```
 * @param { string } body : String of the message.
 */
function parseTemplate (body) {
  let info = {
    summary : '',     /* No use */
    link: '',
    name: '',         /* Name of the repository. */
    association: '',  /* No use */
    upstream: '',     /* No use */
    checklist: '',    /* No use */
  };

  let link_title = '### Direct link to the package repository';

  if (body.lastIndexOf(link_title) !== -1) {
    let lines = body.split('\r\n').filter(function (elm) {
      return elm != '';  // Remove empty string from array.
    });

    info.link = lines[lines.indexOf(link_title) + 1];

    let url_path = info.link.split('/');
    info.name = url_path[url_path.length - 1];
  }

  return info;
}

/**
 * Get PR information by NUM.
 * @param { number } num : PR id/number.
 */
function getPRInfo(num) {
  console.log('[INFO] Get PR %s information', num);
  return new Promise((resolve, reject) => {
    axios.get('https://api.github.com/repos/melpa/melpa/pulls/' + num)
      .then(response => {
        let body = response.data.body;
        let info = parseTemplate(body);

        if (info.link == '') resolve();

        let clone_path = config.REVIEW_PATH + info.name + '/';
        util.cloneProject(info.link, clone_path);

        // TODO: clone project.
        resolve();
      })
      .catch(error => {
        console.log('[ERROR] Error while getting PR info: %s => %s', num, error);
        resolve();
      });
  });
}

/**
 * Make pacakge review if needed to.
 */
function makeReview() {
  return new Promise((resolve, reject) => {
    if (latest_pr_id == data.status.pr_id) {
      console.log("[INFO] No new package PR need to review");
      resolve();
    }

    while (data.status.pr_id < latest_pr_id) {
      ++data.status.pr_id;
      new Promise((resolve, reject) => { return getPRInfo(data.status.pr_id); });
    }

    // PR ID will eventaully be the lastest PR ID.
    data.status.pr_id = latest_pr_id;
    resolve();
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
  }).then(() => {  /* Do pacakge review. */
    return makeReview();
  }).then(() => {  /* Save last status once. */
    //return data.saveStatus();
    return;
  }).then(() => {
    console.log("[INFO] Done review package at time: %s", util.getTimestamp());
  }).catch((err) => {
    console.log("[ERROR] Failed check PR while processing", err);
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
    return util.ensureCommand('git --version', 'Git');
  }).then(() => {  /* Ensure Emacs installed. */
    return util.ensureCommand('emacs --version', 'Emacs');
  }).then(() => {  /* Prepare data file. */
    if (fs.existsSync(config.DATA_PATH))
      return data.loadStatus();
    else
      return data.saveStatus();
  }).then(() => {  /* Register events. */
    return registerEvents();
  }).then(() => {
    console.log("\nDone loading :::\n");
    if (config.DEBUG) doCheckPR();
  }).catch((err) => {
    console.log("\nInitializing fail :::\n%s", err);
  });
}
main();
