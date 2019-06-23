import React from "react";
import "./App.css";
import { BrowserRouter as Router, Route, Switch } from "react-router-dom";
import { HomePage } from "./Pages/HomePage";
import { ChannelPage } from "./Pages/ChannelPage";
import { CreatePage } from "./Pages/CreatePage";
import {
  session,
  ISession,
  defaultSession,
  SessionContext
} from "./Models/Session";
import { OAuthPage } from "./Pages/OAuthPage";

export interface Props {}

export interface State {
  session: ISession;
}

export class App extends React.PureComponent<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {
      session: defaultSession
    };
  }

  componentDidMount() {
    session.sync();

    session.subject.subscribe(session => {
      this.setState({
        session
      });
    });
  }

  render() {
    return (
      <SessionContext.Provider value={this.state.session}>
        <Router>
          <div className="app">
            <Switch>
              <Route path="/" exact component={HomePage} />
              <Route path="/oauth" component={OAuthPage} />
              <Route path="/channels/create" exact component={CreatePage} />
              <Route path="/channels/:id" component={ChannelPage} />
            </Switch>
          </div>
        </Router>
      </SessionContext.Provider>
    );
  }
}

export default App;
