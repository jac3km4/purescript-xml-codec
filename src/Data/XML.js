"use strict";

let parser;
let serializer;
let dom;

if (typeof window === 'undefined') {
  const { DOMParser, XMLSerializer, DOMImplementation } = require('xmldom')

  parser = new DOMParser();
  serializer = new XMLSerializer();
  dom = new DOMImplementation().createDocument("", "");
} else {

  parser = new DOMParser();
  serializer = new XMLSerializer();
  dom = document;
}

exports._parse = (err, succ, str) => {
  const res = parser.parseFromString(str, "text/xml");
  if (res.documentElement && res.documentElement.tagName == "parseerror") {
    return err(res.documentElement.textContent);
  } else {
    return succ(res);
  }
};

exports._stringify = (node) => serializer.serializeToString(node);

exports._getAttribute = (name, el) => el.getAttribute(name) || undefined;
exports._getContent = (el) => el.textContent;
exports._getElementsByTag = (tag, el) => el.getElementsByTagName(tag);
exports._getChildrenByTag = (tag, el) => el.children.filter(e => e.tagName == tag);

exports._createElement = (tag) => dom.createElement(tag);
exports._appendChild = (child, el) => el.appendChild(child);
exports._setAttribute = (name, val, el) => el.setAttribute(name, val);
exports._setTextContent = (text, el) => { el.textContent = text; };
