/**
 * $File: data.js $
 * $Date: 2020-03-09 18:18:38 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

const fs = require('fs');

const config = require('./config');
const util = require('./util');


/* Save data structure. */
var status = {
  pr_id : -1,
  timestamp: ''
};


/**
 * Save status to data file.
 *
 * Keep the status so next time you bootup the service it wouldn't
 * forget what was the state last time we closed.
 */
function saveStatus() {
  status.timestamp = util.getTimestamp();
  return new Promise((resolve, reject) => {
    fs.writeFile(config.DATA_PATH, JSON.stringify(status), function (err) {
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
        /* Put down the data. */
        {
          let data = JSON.parse(rw_data);
          status.pr_id = data.pr_id;
        }
      }
      resolve();
    });
  });
}

//------------------- Module Exports -------------------//

module.exports.status = status;

module.exports.loadStatus = loadStatus;
module.exports.saveStatus = saveStatus;
