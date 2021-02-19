#devtools::install_github("jienagu/noteMD")
#devtools::install_github("ThomasSiegmund/D3TableFilter")
#remotes::install_github("dreamRs/shinypop")
#evtools::install_github("jienagu/nivopie")

library(RSQLite)
library(shinyjs)
library(shinycssloaders)
library(lubridate)
library(shinyFeedback)
library(dplyr)
library(dbplyr)
library(shiny)
library(shinymanager)
library(shinyWidgets)
library(sjlabelled)
library(shinyjs)
library(waiter)
library(sever)
library(shinycustomloader) 
library(shinyLP)
library(shinyBS)
library(DT)
library(readr)
library(shinydashboard)
library(shinypop)
library(forcats)
library(rlang)
library(D3TableFilter)
library(rpivotTable)
library(shiny)
library(dplyr)
library(r2d3)
library(forcats)
library(rlang)
library(shinycssloaders)
library(d3Tree)
library(purrr)
library(stringr)
library(noteMD)
library(webshot)
library(htmlwidgets)
library(memor)
library(nivopie)
library(shinyalert)
library(D3TableFilter)
library(shinyWidgets)
library(bslib)

require(rpivotTable)
require(r2d3)


r2d3_script <- "
// !preview r2d3 data= data.frame(label = c('Austin Bergstrom Intl', 'Chicago Ohare Intl', 'Dallas Fort Worth Intl', 'Eagle Co Rgnl', 'Fort Lauderdale Hollywood Intl', 'General Edward Lawrence Logan Intl'),
//y = c(365, 1455, 7257,  103,  182,  274), x = c('GPT', 'GPT', 'GPT','GPT','GPT','GPT'))

var layer_left      = 0.35;
    layer_left_text = 0.01;
    layer_top       = 0.1;
    layer_height    = 0.85;
    layer_width     = 0.55;

var col_left_text   = width * layer_left_text;

function svg_height() {return parseInt(svg.style('height'))}
function svg_width()  {return parseInt(svg.style('width'))}

function col_top()  {return svg_height() * layer_top; }
function col_left() {return svg_width()  * layer_left;}

function actual_max() {return d3.max(data, function (d) {return d.y; }); }
function col_width()  {return (svg_width() / actual_max()) * layer_width; }
function col_heigth() {return svg_height() / data.length * layer_height; }

var bars = svg.selectAll('rect').data(data);

bars.enter().append('rect')
    .attr('width', function(d) { return d.y * col_width(); })
    .attr('height',col_heigth() * 0.9)
    .attr('y', function(d, i) { return i * col_heigth() + col_top(); })
    .attr('x', col_left())
    .attr('fill', '#c4154f')
    .attr('opacity', function(d) { return d.y / (actual_max()*0.6 ); })
    .attr('tip', function(d) { return (d.y * col_width()) + col_left(); })
    .attr('d', function(d) { return d.x; })
    .on('click', function(){
      Shiny.setInputValue(
        'bar_clicked', 
        d3.select(this).attr('d'),
        {priority: 'event'}
      );
    })    
    .on('mouseover', function(){
        d3.select(this)
          .attr('fill', '#ffb14e');
    })
    .on('mouseout', function(){
        d3.select(this)
          .attr('fill', '#c4154f');
    });

bars.exit().remove();

bars.transition()
  .duration(500)
    .attr('width', function(d) { return d.y * col_width(); })
    .attr('height',col_heigth() * 0.9)
    .attr('y', function(d, i) { return i * col_heigth() + col_top(); })
    .attr('x', col_left())
    .attr('opacity', function(d) { return d.y / (actual_max()*0.6 ); })
    .attr('tip', function(d) { return (d.y * col_width()) + col_left(); });

// Identity labels

var txt = svg.selectAll('text').data(data);

txt.enter().append('text')
      .attr('x', col_left_text)
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .text(function(d) {return d.label; })
      .style('font-size', '14px') 
      .style('font-weight', 'bold') 
      .style('font-family', 'sans-serif');  
      
txt.exit().remove();

txt.transition()
  .duration(1000)
      .attr('x', col_left_text)
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .attr('d', function(d) { return d.x; })
      .style('font-size', '14px') 
      .style('font-weight', 'bold') 
      .style('font-family', 'sans-serif')
      .text(function(d) {return d.label; });  

// Numeric labels

var totals = svg.selectAll().data(data);

totals.enter().append('text')
      .attr('x', function(d) { return ((d.y * col_width()) + col_left()) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .style('font-size', '14px') 
      .style('font-weight', 'bold') 
      .style('font-family', 'sans-serif')
      .text(function(d) {return d.y; });  
      
totals.exit().remove();

totals.transition()
  .duration(1000)
      .attr('x', function(d) { return ((d.y * col_width()) + col_left()) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .attr('d', function(d) { return d.x; })
      .text(function(d) {return d.y; });
      
// Title
      
// svg.append('text')
//  .attr('x', svg_width() * 0.01)             
//  .attr('y', svg_height() * 0.05)
//  .style('font-size', '18px') 
//  .style('font-family', 'sans-serif')
//  .text('Top 10 Destination Airports');
  
// Sub-title
  //https://bl.ocks.org/emmasaunders/raw/0016ee0a2cab25a643ee9bd4855d3464/
svg.append('text')
  .attr('x', svg_width() * 0.99)             
  .attr('y', svg_height() * 0.05)
  .attr('text-anchor', 'start')
  .style('font-size', '16px') 
  .style('font-family', 'sans-serif');
  //.text('Click 'ALL' on tab to reset!');
  //.text('Click bar to zoom into monthly; click 'ALL' on tab to reset!');
  "
r2d3_file <- tempfile()
writeLines(r2d3_script, r2d3_file)

conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  
  dbname = 'data/mtcars.sqlite3'
)

ram<-conn %>% tbl("mtcars") %>%as.data.frame()

shiny::onStop(function() {
  dbDisconnect(conn)
})

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)
