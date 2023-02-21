(function(){
    const container = document.getElementById('container');
    const app = Elm.Simulator.init({node: container});

    app.ports.requestSvg.subscribe(function(){
        const snowflake = document.getElementById('snowflake');
        app.ports.receiveSvg.send(snowflake.outerHTML);
    })
})()