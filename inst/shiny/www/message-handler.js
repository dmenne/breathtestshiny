// This recieves messages of type "testmessage" from the server.
Shiny.addCustomMessageHandler("replace_message",
  function(url) {
    window.location.replace(url);
  }
);
