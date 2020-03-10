/**
 * $File: config.js $
 * $Date: 2020-03-09 00:25:07 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

const DEBUG = true;

// Interval time to do PR review.
const CHECK_TIME = 1000 * 60 * 30;

const APP_NAME = 'package-bot - MELPA';

const DATA_PATH = './status.dat';

// Clone the review package to this path.
const REVIEW_PATH = './review/';

const REVIEW_SCRIPT = './review.el';

//------------------- Module Exports -------------------//

module.exports.DEBUG = DEBUG;
module.exports.CHECK_TIME = CHECK_TIME;
module.exports.APP_NAME = APP_NAME;
module.exports.DATA_PATH = DATA_PATH;
module.exports.REVIEW_PATH = REVIEW_PATH;
module.exports.REVIEW_SCRIPT = REVIEW_SCRIPT;
