
const DATA_URL = "https://raw.githubusercontent.com/alexei-kouprianov/COVID.2019.ru/master/data/momentary.txt";
// const DATA_URL = "/data/momentary.txt";

// Threshold for race graph
const THRES = 50;

const root   = document.getElementById("root");
const race   = document.getElementById("race");

function updateSel(traces) {
  console.log("reset");
  const reset = {
    opacity: 0.1
  };
  Plotly.restyle(root, reset);
  Plotly.restyle(race, reset);

  console.log("collect selection");
  const selected = {
    opacity: 1
  };
  var sel = [];
  for(var i = 0, size = traces.length; i < size; i++) {
    if ($("#c" + i).prop("checked")) sel.push(i);
  }
  console.log(sel);

  Plotly.restyle(root, selected, sel);
  Plotly.restyle(race, selected, sel);

  return false;
}

$(function(){
    //console.log(resp);
  d3.tsv(DATA_URL, d => {
    var date = new Date(d.TIMESTAMP);
    date.setHours(0,0,0,0);
    return { ts: date,
             event: d.EVENT,
             locus: d["LOCUS"],
             num: parseInt(d.NUMBER)
           };
  }).then(raw => {
    var cumul = _.filter(raw, el => el.event == "detected");
    cumul = _.groupBy(cumul, 'locus');
    cumul = _.each(cumul, (v,k) => {
      var r = _.sortBy(v, ["ts"]);
      var start = null;
      if (r[0].num > THRES) {
        start = 0;
        r[0].days = 0;
      }
      for (var i = 1, size = r.length; i < size; ++i) {
        r[i].num += r[i-1].num;
        if (r[i].num > THRES) {
          if (!start) start = i;
          r[i].days = i - start;
        }
      }
      return r;
    });

    const dates = _.sortedUniqBy(_.sortBy(_.flatMap(cumul, (v,k) => _.map(v,'ts')), d => d.getTime()), d => d.getTime());

    const traces = _.sortBy(_.map(cumul, (v,k) => ({
      name: k,
      x: _.map(v, "ts"),
      y: _.map(v, "num")
    })), ["name"]);

    console.log(dates);
    console.log(traces);

    const tracesCfg = {
      xaxis: {
        type: 'date'
      },
      yaxis: {
        type: 'log',
        title: 'COVID-19 cases detected'
      },
      responsive: true
    }

    Plotly.newPlot(root, traces, tracesCfg);

    const races = _.sortBy(_.map(cumul, (v,k) => ({
      name: k,
      x: _.map(v, "days"),
      y: _.map(v, "num")
    })), ["name"]);

    const racesCfg = {
      xaxis: {
        title: 'Days since N=' + THRES + ' threshold'
      },
      yaxis: {
        type: 'log',
        title: 'COVID-19 cases detected'
      },
      responsive: true
    };

    Plotly.newPlot(race, races, racesCfg);

    _.eachRight(traces, (t,i) => {
      $("#checkboxes").prepend('<div class="form-check col-1"><input class="form-check-input region" type="checkbox" value="" id="c' + i + '"><label class="form-check-label" for="c' + i + '">' + t.name + '</label></div>');
    });
    updateSel(traces);
    $(".region").change(e => updateSel(traces));

    $(".plot").on('plotly_legendclick', function (e) {
      console.log(e);
      $("#c" + e.curveNumber)
        .prop("checked",true)
        .change();
      return false;
    });

    $("#reset")
      .click(function(){
        $(".region").prop("checked",false);
        updateSel(traces);
        return false;
      })
      .click();

    $("#loading").hide();
    console.log("done");
    });
});
