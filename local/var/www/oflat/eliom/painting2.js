function start () {
  var  cy = window.cy = cytoscape({
    container: document.getElementById('cy'),
    layout: {
      name: 'grid',
      rows: 2,
      cols: 2
    },
    style: [
      {
        selector: 'node[name]',
        style: {
          'content': 'data(name)',
          'width': '40px',
          'height': '40px',
          'text-valign': 'bottom',
          'text-halign': 'center'
        }
      },
      {
        selector: 'edge[symbol]',
        style: {
          'content': 'data(symbol)'
        }
      },
      {
        selector: 'edge',
        style: {
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle'
        }
      },
    {
      selector: '#transparent',
      style: {
        'visibility': 'hidden'
      }
    },
    {
      selector: '.SUCCESS',
      style: {
        'border-width': '7px',
        'border-color': 'black',
        'border-style': 'double'
      }
  },
    ],
    elements: {
      nodes: [
        {data: { id: 'transparent', name: 'transparent' },  position: { x: 100, y: 100 }}
      ]
    }
  });
}

function makeNode1 (nm, final)  {
  var  verify = cy.getElementById (nm);
  console.log (final);
  if (verify.length < 1) { 
    if (final == "true") {
    if (nm == 'START') {
      console.log ("Teste");
      cy.add({
        data: { id: nm, name: nm },
        position: { x: 100, y: 100 }, classes: 'SUCCESS'
      });
      makeEdge ('transparent', nm, '')
    } else {
        cy.add({
          data: { id: nm, name: nm },
          position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }, classes: 'SUCCESS'
        });
    }
    } else {
      if (nm == 'START') {
        console.log ("Teste");
        cy.add({
          data: { id: nm, name: nm },
          position: { x: 100, y: 100 }
        });
        makeEdge ('transparent', nm, '')
      } else {
          cy.add({
            data: { id: nm, name: nm },
            position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }
          });
      }
    }
    cy.fit();
  }
};

function makeNode (nm, isStart, final)  {
  var  verify = cy.getElementById (nm);
  console.log (final);
  if (verify.length < 1) { 
    if (final == "true") {
    if (isStart == "true") {
      console.log ("isStart = " + isStart);
      cy.add({
        data: { id: nm, name: nm },
        position: { x: 100, y: 100 }, classes: 'SUCCESS'
      });
      makeEdge ('transparent', nm, '')
    } else {
        cy.add({
          data: { id: nm, name: nm },
          position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }, classes: 'SUCCESS'
        });
    }
    } else {
      if (isStart == "true") {
        console.log ("isStart = " + isStart);
        cy.add({
          data: { id: nm, name: nm },
          position: { x: 100, y: 100 }
        });
        makeEdge ('transparent', nm, '')
      } else {
          cy.add({
            data: { id: nm, name: nm },
            position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }
          });
      }
    }
    cy.fit();
  }
};

function makeEdge (first, second, third)  {
  console.log (first + ", " + second + ", " + third);

  var nId = first + second;
  var getEdge = cy.getElementById(nId);

  if (getEdge.length  == 0) {
    cy.add({
      data: { id: nId, source: first, symbol: third, target: second }
    });
  } else {
    var k = getEdge.data('symbol');
    getEdge.remove();
    var newsymbol = k + ', ' + third;
    cy.add({
      data: { id: nId, source: first, symbol: newsymbol, target: second }
    })
  }

  cy.fit();
};

function destroy1 () {
  if (cy != null) {
    cy.destroy1;
  }

};

function fit () {
  if (cy != null) {
    cy.resize();
  }

};

function changeColor1 (node, length, success) {
  console.log (node);
  var test = parseInt(length);
  console.log ("Tamanho da frase" + test);

  console.log (success);

  if (length != 0) {
  cy.style (cytoscape.stylesheet()
    .selector( 'node[name]')
    .style ({
      'content': 'data(name)',
      'text-valign': 'bottom',
      'text-halign': 'center'
    })
    .selector( 'edge[symbol]')
    .style ( {
          'content': 'data(symbol)'
        })
    .selector ('edge')
    .style ({
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle'
    })
    .selector('#' + node)
    .style({
      'background-color': 'blue'
    })
    .selector ('.SUCCESS')
    .style ({
          'border-width': '10px',
          'border-color': 'black',
          'border-style': 'double'
        })
    .selector( '#transparent')
    .style ({
          'visibility': 'hidden'
        })
  );
  } else {
    if (success == "true") {
      cy.style (cytoscape.stylesheet()
    .selector( 'node[name]')
    .style ({
      'content': 'data(name)',
      'text-valign': 'bottom',
      'text-halign': 'center'
    })
    .selector( 'edge[symbol]')
    .style ( {
          'content': 'data(symbol)'
        })
    .selector ('edge')
    .style ({
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle'
    })
    .selector('#' + node)
    .style({
      'background-color': 'green'
    })
    .selector ('.SUCCESS')
    .style ({
          'border-width': '10px',
          'border-color': 'black',
          'border-style': 'double'
        })
    .selector( '#transparent')
    .style ({
              'visibility': 'hidden'
            })
  );
    } else {
      cy.style (cytoscape.stylesheet()
    .selector( 'node[name]')
    .style ({
      'content': 'data(name)',
      'text-valign': 'bottom',
      'text-halign': 'center'
    })
    .selector( 'edge[symbol]')
    .style ( {
          'content': 'data(symbol)'
        })
    .selector ('edge')
    .style ({
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle'
    })
    .selector('#' + node)
    .style({
      'background-color': 'red'
    })
    .selector ('.SUCCESS')
    .style ({
          'border-width': '10px',
          'border-color': 'black',
          'border-style': 'double'
        })
        .selector( '#transparent')
        .style ({
              'visibility': 'hidden'
            })
  );
    }
  }
};

function resetStyle () {
  cy.style()
    .resetToDefault()
    .selector ('node[name]')
    .style ({'content': 'data(name)',
          'width': '40px',
          'height': '40px', 'text-valign': 'bottom',
          'text-halign': 'center'})
    .selector( 'edge[symbol]')
    .style ( {
                'content': 'data(symbol)'
              })
    .selector( 'edge')
    .style ({
            'curve-style': 'bezier',
            'target-arrow-shape': 'triangle'
          })
          .selector ('.SUCCESS')
          .style ({
                'border-width': '10px',
                'border-color': 'black',
                'border-style': 'double'
              })
            .selector( '#transparent')
            .style ({
                    'visibility': 'hidden'
                  })
    .update()
}

function paintNode (node, color) {
  cy.style ()
    .selector('#' + node)
    .style ( {
      'background-color': color
    })
    .update()
}

function myalert(s) { alert("||| " + s + " |||") }

function fileSelectAction() {
  alert ("Teste 1");
	const file = window.event.target.files[0];
	if( file == undefined ) // if canceled
		return;
  const reader = new FileReader();
  window.fileContents = "";
	reader.onload = function(event) { window.fileContents = event.target.result; };
  reader.readAsText(file);

  
}





function start2 () {
  var cy2 = window.cy2 = cytoscape({
    container: document.getElementById('cy2'),
    layout: {
      name: 'grid',
      rows: 2,
      cols: 2
    },
    style: [
      {
        selector: 'node[name]',
        style: {
          'content': 'data(name)',
          'width': '40px',
          'height': '40px',
          'text-valign': 'bottom',
          'text-halign': 'center'
        }
      },
      {
        selector: 'edge[symbol]',
        style: {
          'content': 'data(symbol)'
        }
      },
      {
        selector: 'edge',
        style: {
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle'
        }
      },
    {
      selector: '#transparent',
      style: {
        'visibility': 'hidden'
      }
    },
    {
      selector: '.SUCCESS',
      style: {
        'border-width': '7px',
        'border-color': 'black',
        'border-style': 'double'
      }
  },
    ],
    elements: {
      nodes: [
        {data: { id: 'transparent', name: 'transparent' },  position: { x: 100, y: 100 }}
      ]
    }
  });
}

function makeNode2 (nm, final)  {
  var  verify = cy2.getElementById (nm);
  console.log (final);
  if (verify.length < 1) { 
    if (final == "true") {
    if (nm == 'START') {
      console.log ("Teste");
      cy2.add({
        data: { id: nm, name: nm },
        position: { x: 100, y: 100 }, classes: 'SUCCESS'
      });
      makeEdge ('transparent', nm, '')
    } else {
        cy2.add({
          data: { id: nm, name: nm },
          position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }, classes: 'SUCCESS'
        });
    }
    } else {
      if (nm == 'START') {
        console.log ("Teste");
        cy2.add({
          data: { id: nm, name: nm },
          position: { x: 100, y: 100 }
        });
        makeEdge ('transparent', nm, '')
      } else {
          cy2.add({
            data: { id: nm, name: nm },
            position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }
          });
      }
    }
    cy2.fit();
  }
};

function makeEdge2 (first, second, third)  {
  console.log (first + ", " + second + ", " + third);

  var nId = first + second;
  var getEdge = cy2.getElementById(nId);

  if (getEdge.length  == 0) {
    cy2.add({
      data: { id: nId, source: first, symbol: third, target: second }
    });
  } else {
    var k = getEdge.data('symbol');
    getEdge.remove();
    var newsymbol = k + ', ' + third;
    cy2.add({
      data: { id: nId, source: first, symbol: newsymbol, target: second }
    })
  }

  cy2.fit();
};

function destroy2 () {
  if (cy2 != null) {
    cy2.destroy1;
  }

}; 

function paintNode1 (node, color) {
  cy2.style ()
    .selector('#' + node)
    .style ( {
      'background-color': color
    })
    .update()
}



