import React from "react";
import "./App.css";
import { BrowserRouter as Router, Route, Switch } from "react-router-dom";
import { HomePage } from "./Pages/HomePage";
import { ChannelPage } from "./Pages/ChannelPage";
import { CreatePage } from "./Pages/CreatePage";

const App: React.FC = () => {
  return (
    <Router>
      <div className="app">
        <Switch>
          <Route path="/" exact component={HomePage} />
          <Route path="/channels/create" exact component={CreatePage} />
          <Route path="/channels/:id" component={ChannelPage} />
        </Switch>
      </div>
    </Router>
  );
};

export default App;
