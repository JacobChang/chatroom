import React from "react";
import { Container } from "../Components/Container";
import { Link, RouteProps } from "react-router-dom";
import { ajax } from "rxjs/ajax";

export class OAuthPage extends React.Component<RouteProps> {
  componentDidMount() {
    if (this.props.location) {
      let search = this.props.location.search.slice(1);
      let parts = search.split("&");
      let params: any = {};
      for (let part of parts) {
        let pair = part.split("=");
        if (pair.length === 2) {
          params[pair[0]] = pair[1];
        }
      }

      ajax
        .post(
          "/api/v1/oauth",
          {
            code: params.code,
            state: params.state
          },
          {
            "Content-Type": "application/json"
          }
        )
        .subscribe(next => {
          window.location.href = "/";
        });
    }
  }

  render() {
    return (
      <div className="page page--oauth">
        <Container>
          <p className="text--center">Authorizing...</p>
        </Container>
      </div>
    );
  }
}
