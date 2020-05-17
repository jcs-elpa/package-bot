/**
 * $File: config.js $
 * $Date: 2020-03-09 00:25:07 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2020 by Shen, Jen-Chieh $
 */

"use strict";

const DEBUG           = true;

const APP_NAME        = 'package-bot - MELPA';

// Interval time to do PR review.
const CHECK_TIME      = 1000 * 60 * 30;

const DATA_PATH       = './status.dat';
const REVIEW_PATH     = './review/';            // Clone the review package to this path.
const REVIEW_SCRIPT   = './review.el';
const REQUEST_DATA    = './request.json';       // Temporary file to output the body form.

const OUTPUT_HEADER   = './output/header.txt';  // File contains header information.
const OUTPUT_BODY     = './output/body.txt';    // File contains content information.
const OUTPUT_FOOTER   = './output/footer.txt';  // File contains footer information.

const TEMPLATE_HEADER = './templates/header.md';
const TEMPLATE_BODY   = './templates/body.md';
const TEMPLATE_FOOTER = './templates/footer.md';

const USERNAME        = 'jcs090218';
const ACCESS_TOKEN    = 'bc5ba72b60dd11862d4c5f230c62fed375b6b59a';

//------------------- Module Exports -------------------//

module.exports.DEBUG = DEBUG;
module.exports.CHECK_TIME = CHECK_TIME;
module.exports.APP_NAME = APP_NAME;
module.exports.DATA_PATH = DATA_PATH;
module.exports.REVIEW_PATH = REVIEW_PATH;
module.exports.REVIEW_SCRIPT = REVIEW_SCRIPT;
module.exports.REQUEST_DATA = REQUEST_DATA;
module.exports.OUTPUT_HEADER = OUTPUT_HEADER;
module.exports.OUTPUT_BODY = OUTPUT_BODY;
module.exports.OUTPUT_FOOTER = OUTPUT_FOOTER;
module.exports.TEMPLATE_HEADER = TEMPLATE_HEADER;
module.exports.TEMPLATE_BODY = TEMPLATE_BODY;
module.exports.TEMPLATE_FOOTER = TEMPLATE_FOOTER;
module.exports.USERNAME = USERNAME;
module.exports.ACCESS_TOKEN = ACCESS_TOKEN;
