import React from "react";
import "./HomePage.css";
import { Container } from "../Components/Container";
import { ajax } from "rxjs/ajax";
import { Subscription } from "rxjs";
import { SessionContext } from "../Models/Session";
import { Link } from "react-router-dom";

export class HomePage extends React.Component {
  subscription: Subscription | null = null;

  componentDidMount() {
    ajax("/api/v1/channels").subscribe(channels => {
      console.log(channels);
    });
  }

  render() {
    return (
      <SessionContext.Consumer>
        {session => {
          return (
            <div className="page page--home">
              <header>
                <Container className={["flex__box", "flex__box--vc"]}>
                  <h3 className="flex__item">Hurry</h3>
                  {session.verified ? (
                    <Link to="/channles/create">Create Channel</Link>
                  ) : (
                    <a
                      href={`${
                        process.env.REACT_APP_OAUTH_ENDPOINT
                      }?client_id=${
                        process.env.REACT_APP_OAUTH_CLIENT_ID
                      }&server_id=${
                        process.env.REACT_APP_OAUTH_SERVER_ID
                      }&scope_name=user.id&response_type=code&redirect_uri=${encodeURIComponent(
                        process.env.REACT_APP_OAUTH_REDIRECT_URI as string
                      )}&state=stable`}
                    >
                      Join
                    </a>
                  )}
                </Container>
              </header>
              <section>
                <Container>
                  <p>Home Page</p>
                </Container>
              </section>
            </div>
          );
        }}
      </SessionContext.Consumer>
    );
  }
}
