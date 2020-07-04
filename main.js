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
const util = require('util');
const childProcecss = require("child_process");

const axios = require('axios');

const config = require('./config');
const data = require('./data');
const helper = require('./helper');


// Use to store the newest PR Id. To check if we need to make any
// package review.
var latest_pr_id = -1;

// Use to clean up afterward.
var infos = [];


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
function parseTemplate(body) {
  let info = {
    summary : '',     /* No use */
    link: '',
    association: '',  /* No use */
    upstream: '',     /* No use */
    checklist: '',    /* No use */

    name: '',         /* Name of the repository. */
    clone_path: '',   /* Record down the clone path. */
  };

  let link_title = '### Direct link to the package repository';

  if (body.lastIndexOf(link_title) !== -1) {
    let lines = body.split('\r\n').filter(function (elm) {
      return elm != '';  // Remove empty string from array.
    });

    info.link = lines[lines.indexOf(link_title) + 1];

    let url_path = info.link.split('/');
    let pos_offset = (info.link.slice(-1) == '/') ? 2 : 1;
    info.name = url_path[url_path.length - pos_offset];
  }

  return info;
}

/**
 * Get PR information by NUM.
 * @param { number } num : PR id/number.
 */
function getPRInfo(num) {
  console.log('[INFO] Get PR %s information', num);
  console.log('---------------------------------------------------------');
  return new Promise((resolve, reject) => {
    axios.get('https://api.github.com/repos/melpa/melpa/pulls/' + num)
      .then(response => {
        let body = response.data.body;
        let info = parseTemplate(body);

        if (info.link == '') {
          console.log('[INFO] Detect PR %s is not the new package, no need to review', num);
          resolve();
        }

        infos.push(info);

        info.clone_path = config.REVIEW_PATH + info.name + '/';
        helper.removeDir(info.clone_path);                // Remove before clone.
        helper.cloneProject(info.link, info.clone_path);  // Clone it.

        /* Start review progress for this PR. */
        {
          console.log('[INFO] Starting the review progress..');
          let review_cmd = util.format
          ('emacs --batch --eval "(progn %s)" -l "%s"',
           util.format('%s %s %s %s %s %s %s',
                       util.format('(setq project-dir \\"%s\\")', info.clone_path),
                       util.format('(setq output-header \\"%s\\")', config.OUTPUT_HEADER),
                       util.format('(setq output-body \\"%s\\")', config.OUTPUT_BODY),
                       util.format('(setq output-footer \\"%s\\")', config.OUTPUT_FOOTER),
                       util.format('(setq template-header \\"%s\\")', config.TEMPLATE_HEADER),
                       util.format('(setq template-body \\"%s\\")', config.TEMPLATE_BODY),
                       util.format('(setq template-footer \\"%s\\")', config.TEMPLATE_FOOTER)),
           config.REVIEW_SCRIPT);

          childProcecss.execSync(review_cmd);
        }

        /* Make PR comment. */
        {
          console.log('[INFO] Making package review to the PR..');
          let pr_param = {
            body: '',
          };

          let header = fs.readFileSync(config.OUTPUT_HEADER, 'utf-8');
          let body = fs.readFileSync(config.OUTPUT_BODY, 'utf-8');
          let footer = fs.readFileSync(config.OUTPUT_FOOTER, 'utf-8');
          pr_param.body = header + body + footer;

          fs.writeFileSync(config.REQUEST_DATA, JSON.stringify(pr_param));

          let url = 'https://api.github.com/repos/melpa/melpa/issues/' + num + '/comments';

          if (config.DEBUG)
            url = 'https://api.github.com/repos/jcs-elpa/package-bot/issues/2/comments';

          console.log('[URL] ' + url);

          let comment_cmd = util.format
          ('curl -u %s:%s --header "Content-Type: application/json" --request POST --data \"@%s\" \"%s\"',
           config.USERNAME, config.ACCESS_TOKEN, config.REQUEST_DATA, url);

          childProcecss.execSync(comment_cmd, (error, stdout, stderr) => { });
        }

        resolve();
      })
      .catch(error => {
        console.log('[ERROR] Error while getting PR %s info: %s', num, error);
        resolve();
      });
  });
}

/**
 * Start the review loop.
 */
function startReview() {
  return new Promise((resolve, reject) => {
    resolve();
  }).then(() => {
    ++data.status.pr_id;
    return getPRInfo(data.status.pr_id);
  }).then(() => {
    if (data.status.pr_id < latest_pr_id)
      return startReview();
    // PR ID will eventaully be the lastest PR ID.
    data.status.pr_id = latest_pr_id;
    return 0;
  });
}

/**
 * Make pacakge review if needed to.
 */
function makeReview() {
  return new Promise((resolve, reject) => {
    resolve();
  }).then(() => {
    if (latest_pr_id == data.status.pr_id) {
      console.log("[INFO] No new package PR need to review");
      return 0;
    }
    return startReview();
  });
}

/**
 * Clean up the review process.
 */
function cleanup() {
  return new Promise((resolve, reject) => {
    console.log('[INFO] Cleaning up the process');
    infos.forEach(function (info) {
      helper.removeDir(info.clone_path);
    });
    infos = [];  // clean up the array.
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
    return data.saveStatus();
  }).then(() => {  /* Do clean up. */
    return cleanup();
  }).then(() => {
    console.log("[INFO] Done review package at time: %s", helper.getTimestamp());
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
    return helper.ensureCommand('git --version', 'Git');
  }).then(() => {  /* Ensure Emacs installed. */
    return helper.ensureCommand('emacs --version', 'Emacs');
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
