/**
 * $File: helper.js $
 * $Date: 2020-03-09 23:44:53 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

const fs = require('fs');
const path = require("path");
const childProcecss = require("child_process");


/**
 * Clone a project from URL to PATH.
 * @param { string } url : URL of the project.
 * @param { string } path : Path to clone.
 */
function cloneProject(url, path) {
  let cmd = 'git clone ' + url +  ' ' + path;
  childProcecss.execSync(cmd, (error, stdout, stderr) => {
    console.log('[INFO] Cloning project from %s to %s %s', url, path, stdout);
  });
}

/**
 * Check if the command valid.
 * @param { string } cmd : Command to check installed.
 * @param { string } name : Name of the software to be prompt.
 */
function ensureCommand(cmd, name) {
  return new Promise((resolve, reject) => {
    childProcecss.exec(cmd, (error, stdout, stderr) => {
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

/**
 * Remove directory.
 * @param { string } dirPath : Directory path.
 */
function removeDir(dirPath) {
  if (!fs.existsSync(dirPath)) return;

  let list = fs.readdirSync(dirPath);
  for (var index = 0; index < list.length; ++index) {
    let filename = path.join(dirPath, list[index]);
    let stat = fs.statSync(filename);

    if (filename == "." || filename == "..") {
      // do nothing for current and parent dir
    } else if (stat.isDirectory())
      removeDir(filename);
    else
      fs.unlinkSync(filename);
  }
  fs.rmdirSync(dirPath);
};

//------------------- Module Exports -------------------//

module.exports.cloneProject = cloneProject;
module.exports.ensureCommand = ensureCommand;
module.exports.getTimestamp = getTimestamp;
module.exports.removeDir = removeDir;
