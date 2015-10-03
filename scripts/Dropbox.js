var client = new Dropbox.Client({ key : apiKey });

client.authenticate(function(error, client) {
  if (error) {
    return alert(error);
  }
  console.log("elm-dropbox: Dropbox is authorized");
});

function read(filename) {
  var output = Signal.constant("");

  client.readFile(filename, function(error, data) {
    if (error) {
      return alert(error);
    }
    console.log("elm-dropbox: " + filename + ": Read from Dropbox");
    #{runtime}.notify(output.id, data);
  });

  return output;
}

function write(filename, dataSignal) {
  var isFirst = true;
  var writeToken = null;
  var handler = function(data) {
    if (isFirst) return isFirst = false;
    console.log("elm-dropbox: " + filename + ": writing...", data.slice(0,100));
    if (writeToken) clearTimeout(writeToken);
    writeToken = setTimeout(function() {
      client.writeFile(filename, data, function(error, stat) {
        if (error) {
          return alert(error);
        }
        console.log("elm-dropox: " + filename + ": wrote revision " + stat.versionTag);
      });
    }, 5000);
  }
  Signal.lift(handler)(dataSignal);
}

return { read: read, write: F2(write) };
