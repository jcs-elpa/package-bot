/**
 * $File: util.js $
 * $Date: 2020-03-09 23:44:53 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

const { exec } = require("child_process");


/**
 * Clone a project from URL to PATH.
 * @param { string } url : URL of the project.
 * @param { string } path : Path to clone.
 */
function cloneProject(url, path) {
  let cmd = 'git clone ' + url +  ' ' + path;
  return new Promise((resolve, reject) => {
    exec(cmd, (error, stdout, stderr) => {
      if (error) {
        console.log('[ERROR] Clone project error: %s', error.message);
        resolve();
      }
      if (stderr) {
        console.log('[ERROR] Clone project error: %s', stderr);
        resolve();
      }
      console.log('[INFO] Cloning project from %s to %s,\n\n %s', url, path, stdout);
      resolve();
    });
  });
}

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
 * Get timestamp.
 */
function getTimestamp() {
  return new Date().toISOString().replace(/T/, ' ').replace(/\..+/, '');
}

//------------------- Module Exports -------------------//

module.exports.cloneProject = cloneProject;
module.exports.ensureCommand = ensureCommand;
module.exports.getTimestamp = getTimestamp;
