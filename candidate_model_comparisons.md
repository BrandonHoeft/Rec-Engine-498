Introduction
------------

This document provides model comparison on the test user data for each
of the different final types of recommender systems. Evaluations are
made primarily by using precision, since we have constrained the problem
to a Top 20 recommendations system.

Precision here tells us, of the top 20 recommendations predicted by the
model for for each test user (unseen cases by the trained models), how
many of these were actual parent items that the user did in fact rate?
Higher values indicate that the model does a good job of making relevant
item suggestions to users.

Summary Stats per Model
-----------------------

Distributions of the precision metric on test users for each model type
candidate.

<table>
<thead>
<tr class="header">
<th align="left">model_version</th>
<th align="right">minimum</th>
<th align="right">first_quartile</th>
<th align="right">median</th>
<th align="right">mean</th>
<th align="right">third_quartile</th>
<th align="right">max</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">UBCF &amp; Latent Factor Hybrid</td>
<td align="right">0</td>
<td align="right">0.15</td>
<td align="right">0.20</td>
<td align="right">0.24</td>
<td align="right">0.35</td>
<td align="right">0.80</td>
</tr>
<tr class="even">
<td align="left">UBCF &amp; Popular Hybrid</td>
<td align="right">0</td>
<td align="right">0.15</td>
<td align="right">0.20</td>
<td align="right">0.24</td>
<td align="right">0.35</td>
<td align="right">0.80</td>
</tr>
<tr class="odd">
<td align="left">Popular</td>
<td align="right">0</td>
<td align="right">0.10</td>
<td align="right">0.20</td>
<td align="right">0.23</td>
<td align="right">0.30</td>
<td align="right">0.80</td>
</tr>
<tr class="even">
<td align="left">UBCF</td>
<td align="right">0</td>
<td align="right">0.10</td>
<td align="right">0.20</td>
<td align="right">0.22</td>
<td align="right">0.30</td>
<td align="right">0.85</td>
</tr>
<tr class="odd">
<td align="left">Latent Factor</td>
<td align="right">0</td>
<td align="right">0.05</td>
<td align="right">0.15</td>
<td align="right">0.16</td>
<td align="right">0.25</td>
<td align="right">0.90</td>
</tr>
<tr class="even">
<td align="left">IBCF</td>
<td align="right">0</td>
<td align="right">0.00</td>
<td align="right">0.00</td>
<td align="right">0.05</td>
<td align="right">0.05</td>
<td align="right">0.60</td>
</tr>
<tr class="odd">
<td align="left">Random</td>
<td align="right">0</td>
<td align="right">0.00</td>
<td align="right">0.00</td>
<td align="right">0.01</td>
<td align="right">0.00</td>
<td align="right">0.15</td>
</tr>
</tbody>
</table>

Distributions of the True Positive Count for test users for each model
type candidate. Note, that this information correlates directly to the
precision rate above. It just presents a different number to compare.

<table>
<thead>
<tr class="header">
<th align="left">model_version</th>
<th align="right">minimum</th>
<th align="right">first_quartile</th>
<th align="right">median</th>
<th align="right">mean</th>
<th align="right">third_quartile</th>
<th align="right">max</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">UBCF &amp; Latent Factor Hybrid</td>
<td align="right">0</td>
<td align="right">3</td>
<td align="right">4</td>
<td align="right">4.75</td>
<td align="right">7</td>
<td align="right">16</td>
</tr>
<tr class="even">
<td align="left">UBCF &amp; Popular Hybrid</td>
<td align="right">0</td>
<td align="right">3</td>
<td align="right">4</td>
<td align="right">4.70</td>
<td align="right">7</td>
<td align="right">16</td>
</tr>
<tr class="odd">
<td align="left">Popular</td>
<td align="right">0</td>
<td align="right">2</td>
<td align="right">4</td>
<td align="right">4.60</td>
<td align="right">6</td>
<td align="right">16</td>
</tr>
<tr class="even">
<td align="left">UBCF</td>
<td align="right">0</td>
<td align="right">2</td>
<td align="right">4</td>
<td align="right">4.31</td>
<td align="right">6</td>
<td align="right">17</td>
</tr>
<tr class="odd">
<td align="left">Latent Factor</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">3</td>
<td align="right">3.29</td>
<td align="right">5</td>
<td align="right">18</td>
</tr>
<tr class="even">
<td align="left">IBCF</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0.99</td>
<td align="right">1</td>
<td align="right">12</td>
</tr>
<tr class="odd">
<td align="left">Random</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">0.14</td>
<td align="right">0</td>
<td align="right">3</td>
</tr>
</tbody>
</table>

Visual Comparison of Model Precision of Recommendations
-------------------------------------------------------

Overlapping density plots for each model's precision of recommendations
for the same test users.

![](candidate_model_comparisons_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Overlapping density plots for each model's True Positive Recommendations
count for the same test users.

![](candidate_model_comparisons_files/figure-markdown_strict/unnamed-chunk-6-1.png)

Panel Box Plots of Top 20 Recommendations precision metric for the same
test users.

![](candidate_model_comparisons_files/figure-markdown_strict/unnamed-chunk-7-1.png)

Panel Box Plots of True Positive Recommendations count for the same test
users.

![](candidate_model_comparisons_files/figure-markdown_strict/unnamed-chunk-8-1.png)
