var w = 16, h = 10;
var x_step = 50; var y_step = 50;

var linewidth = 5;
var linecolor = "black";

var colors = [d3.rgb(204, 0, 11),
              d3.rgb(1, 102, 186),
              d3.rgb(249, 213, 26),
              "darkgray",
              "black"];
var colors_weights = [0.5, 0.5, 0.5, 0.25, 0.1];
var colors_cdf = colors_weights.reduce(
    function (acc, w, jj){
        acc.push((acc[jj - 1] || 0) + w); return acc;
    }, [] );


function random_choice(choices, cdf) {
    var r = Math.random() * cdf[cdf.length - 1];
    for (var jj = 0; jj < cdf.length; jj++) {
        if (r <= cdf[jj]) {
            return choices[jj];
        }
    }
}


function shuffle(array) {
  var currentIndex = array.length, temporaryValue, randomIndex;

  while (0 !== currentIndex) {
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}


function random_points(w, h, n) {
    var points = [];
    for (var jj = 1; jj < w; jj++) {
        for (var kk = 1; kk < h; kk++) {
            if (jj != kk) {
                points.push([jj, kk]);
            }
        }
    }
    return shuffle(points).slice(0, n);
}


function kdtree(point_list, axis) {
    if (point_list.length == 0) {
        return null;
    }

    point_list.sort((p1, p2) => p1[axis] - p2[axis]);
    var median = Math.floor(point_list.length / 2);

    var node = {};
    node.location = point_list[median];
    node.axis = axis;

    var new_axis = (axis + 1) % 2;
    node.left_child = kdtree(point_list.slice(0, median), new_axis);
    node.right_child = kdtree(point_list.slice(median + 1), new_axis);

    return node;
}


function kdtree_rects_lines(node, rect) {
    if (node == null) {
        return {rects: [rect], lines: []};
    }

    // Accumulators
    var rect_acc = [];
    var line_acc = [];

    // Rectangle and line variables
    var left_rect = rect.slice();
    var right_rect = rect.slice();
    var line = [[rect[0], rect[2]], [rect[1], rect[3]]];

    // Change variables based on this node
    var split = node.location[node.axis];
    var axis_min = 2 * node.axis; var axis_max = 2 * node.axis + 1;
    if (left_rect[axis_min] == split || right_rect[axis_max] == split) {
        return {rects: [rect], lines: []};
    }
    left_rect[axis_max] = split;
    right_rect[axis_min] = split;
    line[0][node.axis] = split;
    line[1][node.axis] = split;

    line_acc.push(line);

    var lres = kdtree_rects_lines(node.left_child, left_rect);
    var cl_r = lres.rects; var cl_l = lres.lines;
    rect_acc = rect_acc.concat(cl_r);
    line_acc = line_acc.concat(cl_l);

    var rres = kdtree_rects_lines(node.right_child, right_rect);
    var cr_r = rres.rects; var cr_l = rres.lines;
    rect_acc = rect_acc.concat(cr_r);
    line_acc = line_acc.concat(cr_l);

    return {rects: rect_acc, lines: line_acc};
}

var lineFunction = d3.svg.line()
        .x(function(d) { return d.x; })
        .y(function(d) { return d.y; })
        .interpolate("linear");


function point2d (p) {
    return {x: p[0] * x_step, y: p[1] * y_step};
}


function drawlines(the_svg, lines) {
    for (var jj = 0; jj < lines.length; jj++) {
        var line = lines[jj];
        the_svg.append("path")
            .datum(line.map(point2d))
            .attr("d", lineFunction)
            .attr("stroke", linecolor)
            .attr("stroke-width", linewidth)
            .attr("fill", "none");
    }
}


function newImage() {
    var N_rects = document.getElementById("N_rects_input").value;
    if (N_rects == 0) {
        return;
    }
    var N_colored = document.getElementById("N_colored_input").value;

    var svg = d3.select("#chart").append("svg")
            .attr("id", "the_svg")
            .attr("width", w * x_step)
            .attr("height", h * y_step);

    var points_list = random_points(w, h, N_rects - 1);
    var tree = kdtree(points_list, 0);
    var res = kdtree_rects_lines(tree, [0, w, 0, h]);
    var rects = res.rects; var lines = res.lines;

    var colored_rects = shuffle(rects).slice(0, N_colored);

    for (var jj = 0; jj < colored_rects.length; jj++) {
        var rect = colored_rects[jj];
        var color = random_choice(colors, colors_cdf);
        var the_rect = svg.append("rect")
                .attr("x", rect[0] * x_step)
                .attr("y", rect[2] * y_step)
                .attr("width", (rect[1] - rect[0]) * x_step)
                .attr("height", (rect[3] - rect[2]) * y_step)
                .style("stroke", "none")
                .style("fill", color)
                .style("stroke-width", 2 * linewidth);
    }

    var borderPath = svg.append("rect")
            .attr("x", 0)
            .attr("y", 0)
            .attr("width", w * x_step)
            .attr("height", h * y_step)
            .style("stroke", linecolor)
            .style("fill", "none")
            .style("stroke-width", 2 * linewidth);

    drawlines(svg, lines);
}


function updateImage() {
    d3.select("#the_svg").remove();

    newImage();
}


newImage();
