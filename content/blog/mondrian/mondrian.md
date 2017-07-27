Title: Randomized Mondrian-style art generator using D3
Date: 2016-10-01
Scripts: mondrian.js
Styles: mondrian.css

<p>
<form>
    Maximum number of rectangles: <input id="N_rects_input"
                                         type="text"
                                         value=13>
    <br>
    Maximum number of colored rectangles: <input id="N_colored_input"
                                                 type="text"
                                                 value=5>
    <br>
    <input name="drawButton"
           type="button"
           value="New random image"
           onclick="updateImage()" />
</form>
</p>
<p><div id="chart"></div></p>

Uses the k-d tree algorithm from [Wikipedia](https://en.wikipedia.org/wiki/K-d_tree).
