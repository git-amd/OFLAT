var number = 0;

function start () {
  number = 0;
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
        {data: { id: 'transparent', name: 'transparent' }}
      ]
    }
  });
  cy.$('#transparent').position('y', 200);
  cy.$('#transparent').position('x', -200);
  cy.$('#transparent').lock();
}

function generate () {
  return Math.random().toString(36).substr(2, 9);
};

function makeTree (s) {
  var idgen = generate ();
  var str = s;
  var st = str[0];
  console.log (st);
  switch (st) {
    case 'E': return [idgen, "{\"data\": {\"id\": \"" + idgen + "\" , \"name\": \"()\"}}", str.substr(1)];
    case '+': var [lid, lnode, ledge, lret] = makeTree (str.substr(1));
              var [rid, rnode, redge, rret] = makeTree (lret);
              return [idgen, "{\"data\": {\"id\": \"" + idgen + "\", \"name\": \"+\"}}, " + lnode + ", " + rnode, "{\"data\": {\"source\": \"" + idgen + "\", \"target\": \"" + lid + "\"}}, {\"data\": {\"source\": \"" + idgen + "\", \"target\": \"" + rid + "\"}}, " + ledge + ", " + redge, rret];
    case '*': var [cid, cnode, cedge, cret] = makeTree (str.substr(1));
              return [idgen, "{\"data\": {\"id\": \"" + idgen + "\", \"name\": \"*\"}}, " + cnode, "{\"data\": {\"source\": \"" + idgen + "\", \"target\": \"" + cid + "\"}}, " + cedge, cret];
    case '.': var [lid, lnode, ledge, lret] = makeTree (str.substr(1));
              var [rid, rnode, redge, rret] = makeTree (lret);
              return [idgen, "{\"data\": {\"id\": \"" + idgen + "\", \"name\": \".\"}}, " + lnode + ", " + rnode, "{\"data\": {\"source\": \"" + idgen + "\", \"target\": \"" + lid + "\"}}, {\"data\": {\"source\": \"" + idgen + "\", \"target\": \"" + rid + "\"}}, " + ledge + ", " + redge, rret];
    default: return [idgen, "{\"data\": {\"id\": \"" + idgen + "\", \"name\": \"" + st + "\"}}", [], str.substr(1)];
  }
}

var xxx;
var yyy;
var zzz;

function makeTree1 (s) {
  var idgen = generate ();
  var str = s;
  var st = str[0];
  switch (st) {
    case 'E': return [idgen, [{data: {id: idgen, name: "()"}}], [], str.substr(1)];
    case '+': var [lid, lnode, ledge, lret] = makeTree1 (str.substr(1));
              var [rid, rnode, redge, rret] = makeTree1 (lret);
              return [idgen, [{data: {id: idgen, name: "+" }}].concat(lnode).concat(rnode), [{data: {source: idgen, target: lid}}].concat([{data: {source: idgen, target: rid}}]).concat(ledge).concat(redge), rret];
    case '*': var [cid, cnode, cedge, cret] = makeTree1 (str.substr(1));
              return [idgen, [{data: {id: idgen, name: "*"}}].concat(cnode), [{data: {source: idgen, target: cid}}].concat(cedge), cret];
    case '.': var [lid, lnode, ledge, lret] = makeTree1 (str.substr(1));
              var [rid, rnode, redge, rret] = makeTree1 (lret);
              return [idgen, [{data: {id: idgen, name: "."}}].concat(lnode).concat(rnode), [{data: {source: idgen, target: lid}}].concat([{data: {source: idgen, target: rid}}]).concat(ledge).concat(redge), rret];
    default: return [idgen, [{data: {id: idgen, name: st}}], [], str.substr(1)];
  }
}

function startTree(nString) {
  let teste = makeTree1 (nString);
  var cy = window.cy = cytoscape({
    container: document.getElementById('cy'),
    boxSelectionEnabled: false,
    autounselectify: true,
    layout: {
      name: 'dagre'
    },
    style: [
      {
        selector: 'node[name]',
        style: {
          'content': 'data(name)',
          'width': '40px',
          'height': '40px',
          'text-valign': 'center',
          'text-halign': 'center',
          'text-size': '20px'
        }
      },
      {
        selector: 'node',
        style: {
          'background-color': 'white'
        }
      },
      {
        selector: 'edge',
        style: {
          'width': 4,
          'target-arrow-shape': 'triangle',
          'line-color': '#9dbaea',
          'target-arrow-color': '#9dbaea',
          'curve-style': 'bezier'
        }
      }
    ],
    elements: {
      nodes: teste[1],
      edges: teste[2]
    }
  });

}


function makeNode (nm, isStart, final)  {
  var  verify = cy.getElementById (nm);
  console.log (final);
  if (verify.length < 1) { 
    if (final == "true") {
    if (isStart == "true") {
      console.log ("isStart = " + isStart);
      cy.add({
        data: { id: nm, name: nm }, classes: 'SUCCESS'
      });
      cy.$('#' + nm).position('y', 200);
      cy.$('#' + nm).position('x', -100);
      cy.$('#' + nm).lock();
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
          data: { id: nm, name: nm }
        });
        cy.$('#' + nm).position('y', 200);
        cy.$('#' + nm).position('x', -100);
        cy.$('#' + nm).lock();
        makeEdge ('transparent', nm, '')
      } else {
          cy.add({
            data: { id: nm, name: nm },
            position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }
          });
      }
    }
    cy.fit();
    var pos = cy.$('#transparent').position('x');
    console.log ("Transparent x: " + pos);
    var pos1 = cy.$('#transparent').position('y');
    console.log ("Transparent y: " + pos1);
    var pos2 = cy.$('#' + nm).position('x');
    console.log (nm + " x: " + pos2);
    var pos3 = cy.$('#' + nm).position('y');
    console.log (nm + " y: " + pos3);
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
  var  cy2 = window.cy2 = cytoscape({
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
      selector: '#transparent1',
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
        {data: { id: 'transparent1', name: 'transparent1' }}
      ]
    }
  });
  cy2.$('#transparent1').position('y', 200);
  cy2.$('#transparent1').position('x', -200);
  cy2.$('#transparent1').lock();
}

function makeNode2 (nm, isStart, final)  {
  var  verify = cy2.getElementById (nm);
  if (verify.length < 1) { 
    if (final == "true") {
    if (isStart == "true") {
      console.log ("staaaaaaaaaaaaaart");
      cy2.add({
        data: { id: nm, name: nm }, classes: 'SUCCESS'
      });
      cy2.$('#' + nm).position('y', 200);
      cy2.$('#' + nm).position('x', -100);
      cy2.$('#' + nm).lock();
      makeEdge2 ('transparent1', nm, '')
    } else {
        cy2.add({
          data: { id: nm, name: nm },
          position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }, classes: 'SUCCESS'
        });
    }
    } else {
      if (isStart == "true") {
        console.log ("staaaaaaaaaaaaaart 2");
        cy2.add({
          data: { id: nm, name: nm }
        });
        cy2.$('#' + nm).position('y', 200);
        cy2.$('#' + nm).position('x', -100);
        cy2.$('#' + nm).lock();
        makeEdge2 ('transparent1', nm, '')
      } else {
          cy2.add({
            data: { id: nm, name: nm },
            position: { x: Math.floor(Math.random() * 1399), y: Math.floor(Math.random() * 299) }
          });
      }
    }
    cy2.fit();
    var pos = cy2.$('#transparent1').position('x');
    console.log ("Transparent x: " + pos);
    var pos1 = cy2.$('#transparent1').position('y');
    console.log ("Transparent y: " + pos1);
    var pos2 = cy2.$('#' + nm).position('x');
    console.log (nm + " x: " + pos2);
    var pos3 = cy2.$('#' + nm).position('y');
    console.log (nm + " y: " + pos3);
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

function makeTreeNode (nm)  {
  var  verify = cy.getElementById (nm);
  var newId = 'n' + number;
  console.log (newId);
  if (verify.length < 1) { 
    cy.add({
      data: { id: newId, name: nm }
    });
  };
  cy.fit();
};

function makeTreeEdge (first, second)  {
  var getEdge = cy.getElementById(nId);

  if (getEdge.length  == 0) {
    cy2.add({
      data: { source: first, target: second }
    });
  }
  cy.fit();
};



