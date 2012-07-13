#!/usr/bin/env node

var fs = require("fs");
var path = require("path");
var SFNode = require("../Implementations/JavaScript/SelfML.js");

fs.readdir("./Testcases", function(err, files) {
	if (err) {
		throw err;
	}

	files.sort();

	(function next() {
		var file = files.shift();
		if (!file) return;

		console.log("=== Testing %s ===", file);

		var failure = false;
		var testcase_path = path.join("Testcases", file);
		var output_path = path.join("Outputs", "output." + file);

		fs.readFile(testcase_path, "utf8", function(err, data) {
			if (err) {
				throw error;
			}

			var t_image = SFNode.createFromString(data).toString();
			var t_image2 = SFNode.createFromString(t_image).toString();
			if (t_image !== t_image2) {
				console.log("\t%s failed image test.", file);
				failure = true;
			}

			fs.readFile(output_path, "utf8", function(err, data) {
				if (err) {
					throw error;
				}

				// Remove trailing whitespace in file
				data = data.replace(/\n$/, "");

				var o_image = SFNode.createFromString(data).toString();

				if (t_image !== data) {
					console.log("\t%s failed output test.", file);
					failure = true;
				}

				if (t_image !== o_image) {
					console.log("\t%s failed output image test.", file);
					failure = true;
				}

				var o_image2 = SFNode.createFromString(o_image).toString();
				if (t_image !== o_image2) {
					console.log("\t%s failed output double image test.", file);
					failure = true;
				}

				if (!failure) {
					console.log("\tNO ERRORS");
				}

				console.log("");

				next();
			});
		});
	}());
});
/*
try {
	var input;
	tests.forEach(function(test, i) {
		var result;
		input = test;
		var node = SFNode.createFromString(input);
		result = node.toString();
		if (result != outputs[i]) {
			console.log(result);
			console.log("---");
			console.log(outputs[i]);
			console.log("---");
		}
	});
} catch (e) {
	var index = Number(e.message.match(/\d+/));
	console.log(e);
	console.log(input.slice(index - 10, index + 10));
	console.log(Array(11).join("-") + "^");

	console.log("\n" + e.stack);
}*/
