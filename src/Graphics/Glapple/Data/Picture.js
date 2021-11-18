"use strict";

exports.setGradientStrokeStyle = function(ctx) {
  return function(gradient) {
      return function() {
          ctx.strokeStyle = gradient;
      };
  };
};

exports.setPatternStrokeStyle = function(ctx) {
  return function(pattern) {
      return function() {
          ctx.strokeStyle = pattern;
      };
  };
};